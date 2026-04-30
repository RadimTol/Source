#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(stringr)
  library(readr)
  library(tibble)
  library(xml2)
  library(tidyr)
  library(rvest)
  library(jsonlite)
})

options(stringsAsFactors = FALSE)

default_keywords_file        <- "keywords.txt"
default_output_file          <- "source.csv"
default_crossref_output_file <- "source-crossref.csv"
default_log_file             <- "news_monitor.log"
api_contact_email            <- Sys.getenv("SCHOLARLY_API_EMAIL", unset = "")

rss_feeds <- tribble(
  ~source,              ~url,
  "CT24",               "https://ct24.ceskatelevize.cz/rss/hlavni-zpravy",
  "iDNES",              "https://servis.idnes.cz/rss.aspx?c=zpravodaj",
  "Novinky",            "https://www.novinky.cz/rss",
  "Seznam Zpravy",      "https://www.seznamzpravy.cz/rss",
  "Denik N",            "https://denikn.cz/feed/",
  "Reflex",             "https://www.reflex.cz/rss",
  "Respekt",            "https://www.respekt.cz/rss",
  "BBC World",          "http://feeds.bbci.co.uk/news/world/rss.xml",
  "The Guardian World", "https://www.theguardian.com/world/rss",
  "NYTimes World",      "https://rss.nytimes.com/services/xml/rss/nyt/World.xml",
  "NPR World",          "https://feeds.npr.org/1004/rss.xml",
  "Al Jazeera",         "https://www.aljazeera.com/xml/rss/all.xml",
  "Sky News World",     "https://feeds.skynews.com/feeds/rss/world.xml"
)

science_rss_feeds <- tribble(
  ~source,                         ~url,
  "Nature Climate",              "https://www.nature.com/nclimate.rss",
  "Nature Sustainability",       "https://www.nature.com/natsustain.rss",
  "PNAS",                        "https://www.pnas.org/action/showFeed?feed=rss&jc=PNAS&type=etoc",
  "ScienceDaily Climate",        "https://www.sciencedaily.com/rss/earth_climate/climate.xml",
  "ScienceDaily Global Warming", "https://www.sciencedaily.com/rss/earth_climate/global_warming.xml",
  "ScienceDaily Environmental Issues", "https://www.sciencedaily.com/rss/earth_climate/environmental_issues.xml"
)

science_search_terms <- c(
  "climate change",
  "global warming",
  "climate extremes",
  "environmental change",
  "greenhouse gas emissions",
  "drought",
  "flood",
  "heatwave",
  "biodiversity loss",
  "air pollution"
)

log_file <- default_log_file

empty_items_tbl <- function() {
  tibble(
    source = character(),
    title = character(),
    link = character(),
    summary_raw = character(),
    date = as.Date(character()),
    link_norm = character()
  )
}

log_msg <- function(level, text) {
  line <- sprintf("[%s] [%s] %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), level, text)
  cat(line, "\n")
  cat(line, "\n", file = log_file, append = TRUE)
}

args <- commandArgs(trailingOnly = TRUE)
arg1 <- if (length(args) >= 1) trimws(args[1]) else ""
arg2 <- if (length(args) >= 2) trimws(args[2]) else ""

date_from <- if (nzchar(arg1)) as.Date(arg1) else Sys.Date() - 1
date_to   <- if (nzchar(arg2)) as.Date(arg2) else Sys.Date()

if (is.na(date_from) || is.na(date_to)) stop("Datum musi byt ve formatu YYYY-MM-DD.")
if (date_to < date_from) stop("Koncove datum nesmi byt mensi nez pocatecni datum.")

cat("", file = log_file, append = FALSE)
log_msg("INFO", sprintf("Interval: %s -> %s", date_from, date_to))

if (nzchar(api_contact_email)) {
  log_msg("INFO", sprintf("Pro Crossref bude pouzit kontakt %s", api_contact_email))
} else {
  log_msg("INFO", "SCHOLARLY_API_EMAIL neni nastaven. Crossref pobezi bez identifikace kontaktu.")
}

strip_html <- function(x) {
  x <- ifelse(is.na(x), "", x)
  vapply(
    x,
    function(one) {
      one <- trimws(one)
      if (!nzchar(one)) return("")
      out <- tryCatch(
        xml2::xml_text(xml2::read_html(paste0("<div>", one, "</div>"))),
        error = function(e) one
      )
      stringr::str_squish(out)
    },
    character(1)
  )
}

normalize_link <- function(x) {
  x <- ifelse(is.na(x), "", x)
  x <- trimws(x)
  x <- str_replace(x, "#.*$", "")
  x <- str_replace_all(x, "([?&])utm_[^=]+=[^&]*", "\\1")
  x <- str_replace_all(x, "[?&]+$", "")
  x <- str_replace(x, "\\?$", "")
  x <- str_replace(x, "/$", "")
  x
}

escape_regex <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  stringr::str_replace_all(x, "([.|()\\^{}+$*?\\[\\]\\\\])", "\\\\\\1")
}

first_sentences <- function(x, max_sentences = 3, max_chars = 550) {
  x <- strip_html(x)

  vapply(
    x,
    function(one) {
      if (is.na(one) || one == "") return("")
      parts <- unlist(strsplit(one, "(?<=[.!?])[[:space:]]+", perl = TRUE))
      parts <- trimws(parts)
      parts <- parts[nzchar(parts)]
      if (length(parts) == 0) return("")
      abstract <- paste(head(parts, max_sentences), collapse = " ")
      abstract <- str_squish(abstract)
      if (nchar(abstract) > max_chars) {
        abstract <- paste0(substr(abstract, 1, max_chars - 3), "...")
      }
      abstract
    },
    character(1)
  )
}

read_keywords <- function(path) {
  if (!file.exists(path)) stop(sprintf("Soubor '%s' nebyl nalezen.", path))

  raw <- readLines(path, warn = FALSE, encoding = "UTF-8")
  raw <- trimws(raw)
  raw <- raw[nzchar(raw)]
  if (length(raw) == 0) stop("Soubor keywords.txt je prazdny.")

  parse_line <- function(line) {
    parts <- strsplit(line, "-", fixed = TRUE)[[1]]
    if (length(parts) < 2) return(NULL)

    group_name <- str_squish(parts[1])
    keywords_part <- paste(parts[-1], collapse = "-")
    keywords_part <- str_squish(keywords_part)

    if (!nzchar(group_name) || !nzchar(keywords_part)) return(NULL)

    keywords <- strsplit(keywords_part, ";", fixed = TRUE)[[1]]
    keywords <- str_squish(keywords)
    keywords <- keywords[nzchar(keywords)]

    if (length(keywords) == 0) return(NULL)

    tibble(keyword_group = group_name, keyword_term = keywords)
  }

  out <- purrr::map(raw, parse_line) %>% purrr::compact() %>% bind_rows()

  if (nrow(out) == 0) {
    stop("V keywords.txt nebyly nalezeny validni skupiny ve formatu 'Nazev-klicove_slovo_1;klicove_slovo_2;...'.")
  }

  out %>% distinct(keyword_group, keyword_term)
}

parse_pub_datetime <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""

  normalize_tz <- function(s) {
    s <- trimws(s)
    tz_map <- c(
      GMT = "+0000", UTC = "+0000",
      BST = "+0100", CET = "+0100", CEST = "+0200",
      EST = "-0500", EDT = "-0400",
      CST = "-0600", CDT = "-0500",
      MST = "-0700", MDT = "-0600",
      PST = "-0800", PDT = "-0700"
    )
    for (abbr in names(tz_map)) {
      s <- sub(paste0(" ", abbr, "$"), paste0(" ", tz_map[[abbr]]), s)
    }
    s
  }

  parse_one <- function(s) {
    s <- normalize_tz(s)
    if (!nzchar(s)) return(as.POSIXct(NA, tz = "UTC"))

    fmts <- c(
      "%a, %d %b %Y %H:%M:%S %z",
      "%a, %d %b %Y %H:%M %z",
      "%d %b %Y %H:%M:%S %z",
      "%d %b %Y %H:%M %z",
      "%Y-%m-%dT%H:%M:%S%z",
      "%Y-%m-%dT%H:%M:%S%Ez",
      "%Y-%m-%dT%H:%M:%SZ",
      "%Y-%m-%d %H:%M:%S",
      "%Y-%m-%d"
    )

    for (fmt in fmts) {
      dt <- suppressWarnings(as.POSIXct(strptime(s, fmt, tz = "UTC")))
      if (!is.na(dt)) return(dt)
    }

    as.POSIXct(NA, tz = "UTC")
  }

  as.POSIXct(vapply(x, parse_one, as.POSIXct(NA, tz = "UTC")), origin = "1970-01-01", tz = "UTC")
}

extract_item_text <- function(node, xpath_candidates) {
  vals <- character(0)
  for (xp in xpath_candidates) {
    found <- xml2::xml_find_all(node, xp)
    if (length(found) > 0) {
      vals <- xml2::xml_text(found)
      vals <- vals[!is.na(vals)]
      vals <- trimws(vals)
      vals <- vals[nzchar(vals)]
      if (length(vals) > 0) return(vals[1])
    }
  }
  ""
}

safe_read_xml_url <- function(url) xml2::read_xml(url)
safe_read_json_url <- function(url) jsonlite::fromJSON(url, flatten = TRUE)

fetch_feed <- function(name, url) {
  log_msg("INFO", paste("Loading", name))

  tryCatch({
    feed <- safe_read_xml_url(url)
    items <- xml2::xml_find_all(feed, ".//*[local-name()='item']")
    if (length(items) == 0) items <- xml2::xml_find_all(feed, ".//*[local-name()='entry']")
    if (length(items) == 0) {
      log_msg("WARN", paste("Skipping", name, "- no items found"))
      return(empty_items_tbl())
    }

    get_link_atom <- function(item) {
      href <- xml2::xml_attr(xml2::xml_find_first(item, ".//*[local-name()='link']"), "href")
      if (!is.na(href) && nzchar(href)) return(href)
      extract_item_text(item, c(".//*[local-name()='link']"))
    }

    out <- tibble(
      source = name,
      title = vapply(items, function(item) extract_item_text(item, c(".//*[local-name()='title']")), character(1)),
      link = vapply(items, get_link_atom, character(1)),
      summary_raw = vapply(items, function(item) {
        extract_item_text(item, c(
          ".//*[local-name()='description']",
          ".//*[local-name()='summary']",
          ".//*[local-name()='encoded']",
          ".//*[local-name()='content']"
        ))
      }, character(1)),
      pub_raw = vapply(items, function(item) {
        extract_item_text(item, c(
          ".//*[local-name()='pubDate']",
          ".//*[local-name()='published']",
          ".//*[local-name()='updated']",
          ".//*[local-name()='date']"
        ))
      }, character(1))
    ) %>%
      mutate(
        title = str_squish(strip_html(title)),
        link = str_trim(link),
        summary_raw = str_squish(strip_html(summary_raw)),
        datetime = parse_pub_datetime(pub_raw),
        date = as.Date(datetime),
        link_norm = normalize_link(link)
      ) %>%
      filter(!is.na(link), nzchar(link)) %>%
      select(source, title, link, summary_raw, date, link_norm)

    bad_dates <- sum(is.na(out$date))
    if (bad_dates > 0) {
      log_msg("WARN", sprintf("%s: nepodarilo se naparsovat datum u %d zaznamu", name, bad_dates))
    }
    out
  }, error = function(e) {
    log_msg("WARN", paste("Skipping", name, e$message))
    empty_items_tbl()
  })
}

build_crossref_url <- function(term, date_from, date_to, rows = 100) {
  base <- "https://api.crossref.org/works"
  filter_parts <- c(
    sprintf("from-pub-date:%s", date_from),
    sprintf("until-pub-date:%s", date_to),
    "type:journal-article"
  )
  params <- c(
    paste0("query.bibliographic=", URLencode(term, reserved = TRUE)),
    paste0("filter=", URLencode(paste(filter_parts, collapse = ","), reserved = TRUE)),
    paste0("rows=", rows),
    "select=DOI,URL,title,abstract,published-print,published-online,created,container-title,subject"
  )
  if (nzchar(api_contact_email)) params <- c(params, paste0("mailto=", URLencode(api_contact_email, reserved = TRUE)))
  paste0(base, "?", paste(params, collapse = "&"))
}

is_climate_subject <- function(subjects) {
  if (is.null(subjects) || length(subjects) == 0) return(FALSE)
  txt <- str_to_lower(paste(unlist(subjects), collapse = " "))
  str_detect(txt, "climat|environment|ecolog|sustainab|atmospher|pollution|biodivers|carbon|energy|hydrolog|water")
}

fetch_crossref_term <- function(term, date_from, date_to) {
  log_msg("INFO", sprintf("Crossref query: %s", term))

  tryCatch({
    x <- safe_read_json_url(build_crossref_url(term, date_from, date_to))
    items <- x$message$items
    if (is.null(items) || length(items) == 0) return(empty_items_tbl())
    items <- tibble::as_tibble(items)
    n_items <- nrow(items)
    if (is.null(n_items) || n_items == 0) return(empty_items_tbl())

    pick_date <- function(row) {
      for (col in c("published.print.date-parts", "published.online.date-parts", "created.date-parts")) {
        if (!col %in% names(row)) next
        val <- row[[col]][[1]]
        if (length(val) >= 1) {
          year <- as.integer(val[1])
          month <- ifelse(length(val) >= 2, as.integer(val[2]), 1L)
          day <- ifelse(length(val) >= 3, as.integer(val[3]), 1L)
          return(as.Date(sprintf("%04d-%02d-%02d", year, month, day)))
        }
      }
      as.Date(NA)
    }

    out <- tibble(
      title = if ("title" %in% names(items)) purrr::map_chr(items$title, ~ if (length(.x) > 0) as.character(.x[[1]]) else "") else rep("", n_items),
      summary_raw = if ("abstract" %in% names(items)) strip_html(as.character(items$abstract)) else rep("", n_items),
      date = vapply(seq_len(n_items), function(i) as.character(pick_date(as.list(items[i, , drop = FALSE]))), character(1)),
      source = {
        container_col <- intersect(c("container.title", "container-title"), names(items))
        if (length(container_col) > 0) {
          ct <- purrr::map_chr(items[[container_col[[1]]]], ~ {
            if (length(.x) > 0) as.character(.x[[1]]) else ""
          })
          ct <- str_squish(strip_html(ct))
          ifelse(nzchar(ct), paste0("Crossref/", ct), "Crossref")
        } else {
          rep("Crossref", n_items)
        }
      },
      link = dplyr::coalesce(if ("URL" %in% names(items)) as.character(items$URL) else rep(NA_character_, n_items), if ("DOI" %in% names(items)) ifelse(!is.na(items$DOI) & nzchar(items$DOI), paste0("https://doi.org/", items$DOI), NA_character_) else rep(NA_character_, n_items)),
      climate_ok = if ("subject" %in% names(items)) purrr::map_lgl(items$subject, is_climate_subject) else rep(TRUE, n_items)
    ) %>%
      mutate(
        title = str_squish(strip_html(title)),
        date = as.Date(date),
        link_norm = normalize_link(link)
      ) %>%
      filter(climate_ok, !is.na(date), date >= date_from, date <= date_to, !is.na(link), nzchar(link)) %>%
      select(source, title, link, summary_raw, date, link_norm)

    if (nrow(out) == 0) return(empty_items_tbl())
    out
  }, error = function(e) {
    log_msg("WARN", paste("Crossref failed for", term, "-", e$message))
    empty_items_tbl()
  })
}

build_arxiv_url <- function(term, max_results = 50) {
  paste0(
    "http://export.arxiv.org/api/query?search_query=all:", URLencode(term, reserved = TRUE),
    "&start=0&max_results=", max_results,
    "&sortBy=submittedDate&sortOrder=descending"
  )
}

fetch_arxiv_term <- function(term, date_from, date_to) {
  log_msg("INFO", sprintf("arXiv query: %s", term))

  tryCatch({
    feed <- safe_read_xml_url(build_arxiv_url(term))
    entries <- xml2::xml_find_all(feed, ".//*[local-name()='entry']")
    if (length(entries) == 0) return(empty_items_tbl())

    get_text <- function(node, name) {
      val <- xml2::xml_text(xml2::xml_find_first(node, paste0(".//*[local-name()='", name, "']")))
      ifelse(is.na(val), "", val)
    }
    get_link <- function(node) {
      link_node <- xml2::xml_find_first(node, ".//*[local-name()='link'][@rel='alternate']")
      href <- xml2::xml_attr(link_node, "href")
      if (!is.na(href) && nzchar(href)) return(href)
      get_text(node, "id")
    }

    out <- tibble(
      source = "arXiv",
      title = vapply(entries, function(e) get_text(e, "title"), character(1)),
      link = vapply(entries, get_link, character(1)),
      summary_raw = vapply(entries, function(e) get_text(e, "summary"), character(1)),
      date = as.Date(vapply(entries, function(e) get_text(e, "published"), character(1))),
      link_norm = normalize_link(vapply(entries, get_link, character(1)))
    ) %>%
      mutate(title = str_squish(strip_html(title)), summary_raw = str_squish(strip_html(summary_raw))) %>%
      filter(!is.na(date), date >= date_from, date <= date_to, !is.na(link), nzchar(link)) %>%
      select(source, title, link, summary_raw, date, link_norm)

    if (nrow(out) == 0) return(empty_items_tbl())
    out
  }, error = function(e) {
    log_msg("WARN", paste("arXiv failed for", term, "-", e$message))
    empty_items_tbl()
  })
}

fetch_science_api_results <- function(date_from, date_to) {
  all_terms <- unique(science_search_terms)
  out <- purrr::map_dfr(all_terms, function(term) {
    Sys.sleep(0.2)
    bind_rows(
      fetch_crossref_term(term, date_from, date_to),
      fetch_arxiv_term(term, date_from, date_to)
    )
  })

  if (nrow(out) == 0) return(empty_items_tbl())
  out %>% distinct(link_norm, title, source, .keep_all = TRUE)
}

extract_article_text <- function(link) {
  if (is.na(link) || !nzchar(link)) return("")
  if (str_detect(link, "doi\\.org|arxiv\\.org")) return("")

  if (str_detect(link, "respekt\\.cz|reflex\\.cz")) return("")

  tryCatch({
    page <- read_html(link)
    selectors <- c("article p", "main p", ".article p", ".article__content p", ".entry-content p", ".post-content p", ".content p", "p")
    for (css in selectors) {
      nodes <- html_elements(page, css)
      if (length(nodes) == 0) next
      txt <- html_text2(nodes)
      txt <- str_squish(txt)
      txt <- txt[nchar(txt) > 40]
      txt <- txt[!str_detect(txt, "^(Copyright|All rights reserved|Subscribe|Sign up|Read more)$")]
      if (length(txt) >= 3) return(paste(txt, collapse = " "))
    }
    ""
  }, error = function(e) {
    log_msg("WARN", paste("Article fallback failed for", link, "-", e$message))
    ""
  })
}

build_abstract <- function(summary_raw, link, max_sentences = 3, max_chars = 550) {
  out <- first_sentences(summary_raw, max_sentences = max_sentences, max_chars = max_chars)
  need_fallback <- is.na(out) | out == "" | nchar(out) < 80
  if (any(need_fallback)) {
    idx <- which(need_fallback)
    for (i in idx) {
      article_text <- extract_article_text(link[[i]])
      if (nzchar(article_text)) out[[i]] <- first_sentences(article_text, max_sentences = max_sentences, max_chars = max_chars)
    }
  }
  out
}

match_keywords <- function(news_df, keywords_df) {
  if (nrow(news_df) == 0) return(tibble())

  news_df %>%
    mutate(search_text = str_to_lower(paste(title, summary_raw, sep = " "))) %>%
    tidyr::crossing(keywords_df) %>%
    mutate(matched = str_detect(search_text, regex(paste0("\\b", escape_regex(keyword_term), "\\b"), ignore_case = TRUE))) %>%
    filter(matched) %>%
    mutate(keyword = keyword_group) %>%
    distinct(link_norm, keyword, .keep_all = TRUE)
}

read_existing_results <- function(path) {
  if (!file.exists(path)) {
    return(tibble(date = as.Date(character()), keyword = character(), source = character(), link = character(), abstract = character(), link_norm = character()))
  }

  tryCatch({
    df <- suppressMessages(readr::read_csv(path, show_col_types = FALSE))
    required_cols <- c("date", "keyword", "source", "link", "abstract")
    missing_cols <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0) {
      log_msg("WARN", sprintf("Existujici %s nema ocekavane sloupce, bude ignorovan.", path))
      return(tibble(date = as.Date(character()), keyword = character(), source = character(), link = character(), abstract = character(), link_norm = character()))
    }

    df %>% transmute(date = as.Date(date), keyword = as.character(keyword), source = as.character(source), link = as.character(link), abstract = as.character(abstract), link_norm = normalize_link(link))
  }, error = function(e) {
    log_msg("WARN", sprintf("Existujici %s se nepodarilo nacist, bude ignorovan: %s", path, e$message))
    tibble(date = as.Date(character()), keyword = character(), source = character(), link = character(), abstract = character(), link_norm = character())
  })
}

merge_results <- function(new_results, output_path) {
  existing_results <- read_existing_results(output_path)
  log_msg("INFO", sprintf("Existujici vystup obsahuje %d zaznamu", nrow(existing_results)))
  log_msg("INFO", sprintf("Novy beh vytvoril %d zaznamu", nrow(new_results)))

  combined <- bind_rows(new_results, existing_results) %>%
    mutate(date = as.Date(date), keyword = as.character(keyword), source = as.character(source), link = as.character(link), abstract = as.character(abstract), link_norm = normalize_link(link)) %>%
    arrange(desc(date), source, keyword) %>%
    distinct(link_norm, keyword, .keep_all = TRUE) %>%
    arrange(desc(date), source, keyword)

  added_count <- nrow(combined) - nrow(existing_results)
  if (added_count < 0) added_count <- 0

  log_msg("INFO", sprintf("Po slouceni a deduplikaci je ve vystupu %d zaznamu", nrow(combined)))
  log_msg("INFO", sprintf("Pribylo %d novych zaznamu", added_count))
  combined
}

keywords <- read_keywords(default_keywords_file)
log_msg("INFO", sprintf("Nacteno %d klicovych termu v %d skupinach z %s", nrow(keywords), n_distinct(keywords$keyword_group), default_keywords_file))

all_news <- purrr::map2_dfr(rss_feeds$source, rss_feeds$url, fetch_feed)
log_msg("INFO", sprintf("Nacteno celkem %d zaznamu ze zpravodajskych RSS", nrow(all_news)))

all_science_rss <- purrr::map2_dfr(science_rss_feeds$source, science_rss_feeds$url, fetch_feed)
log_msg("INFO", sprintf("Nacteno celkem %d zaznamu z vedeckych RSS", nrow(all_science_rss)))

all_science_api <- fetch_science_api_results(date_from, date_to)
log_msg("INFO", sprintf("Nacteno celkem %d zaznamu z vedeckych API", nrow(all_science_api)))

all_items <- bind_rows(all_news, all_science_rss, all_science_api) %>% mutate(date = as.Date(date))

if (nrow(all_items) == 0) {
  log_msg("WARN", "Nepodarilo se nacist zadna data ani ze zpravodajskych, ani z vedeckych zdroju.")

  existing_results <- read_existing_results(default_output_file) %>%
    filter(!str_detect(source, "^Crossref")) %>%
    select(date, keyword, source, link, abstract)
  readr::write_excel_csv(existing_results, default_output_file)

  existing_crossref_results <- read_existing_results(default_crossref_output_file) %>%
    select(date, keyword, source, link, abstract)
  readr::write_excel_csv(existing_crossref_results, default_crossref_output_file)

  quit(save = "no")
}

filtered_items <- all_items %>% filter(!is.na(date)) %>% filter(date >= date_from, date <= date_to)
log_msg("INFO", sprintf("Po filtraci na interval zustalo %d zaznamu", nrow(filtered_items)))

if (nrow(filtered_items) == 0) {
  log_msg("WARN", "V danem intervalu nebyly nalezeny zadne zaznamy.")

  existing_results <- read_existing_results(default_output_file) %>%
    filter(!str_detect(source, "^Crossref")) %>%
    select(date, keyword, source, link, abstract)
  readr::write_excel_csv(existing_results, default_output_file)

  existing_crossref_results <- read_existing_results(default_crossref_output_file) %>%
    select(date, keyword, source, link, abstract)
  readr::write_excel_csv(existing_crossref_results, default_crossref_output_file)

  quit(save = "no")
}

results <- match_keywords(filtered_items, keywords)
log_msg("INFO", sprintf("Po matchi na klicova slova: %d zaznamu", nrow(results)))

if (nrow(results) > 0) {
  results <- results %>%
    mutate(abstract = build_abstract(summary_raw, link)) %>%
    arrange(desc(date), keyword, source) %>%
    distinct(link_norm, keyword, .keep_all = TRUE) %>%
    transmute(date = as.Date(date), keyword = keyword, source = source, link = link, abstract = abstract, link_norm = link_norm)
} else {
  results <- tibble(date = as.Date(character()), keyword = character(), source = character(), link = character(), abstract = character(), link_norm = character())
}

crossref_results <- results %>%
  filter(str_detect(source, "^Crossref"))

non_crossref_results <- results %>%
  filter(!str_detect(source, "^Crossref"))

final_results <- merge_results(non_crossref_results, default_output_file) %>%
  filter(!str_detect(source, "^Crossref")) %>%
  select(date, keyword, source, link, abstract)

readr::write_excel_csv(final_results, default_output_file)
log_msg("INFO", sprintf("Hotovo. Ulozeno %d zaznamu do %s", nrow(final_results), default_output_file))

final_crossref_results <- merge_results(crossref_results, default_crossref_output_file) %>%
  select(date, keyword, source, link, abstract)

readr::write_excel_csv(final_crossref_results, default_crossref_output_file)
log_msg("INFO", sprintf("Hotovo. Ulozeno %d zaznamu do %s", nrow(final_crossref_results), default_crossref_output_file))
