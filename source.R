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
})

options(stringsAsFactors = FALSE)

default_keywords_file <- "keywords.txt"
default_output_file   <- "source.csv"
default_log_file      <- "news_monitor.log"

rss_feeds <- tribble(
  ~source,              ~url,
  "CT24",               "https://ct24.ceskatelevize.cz/rss/hlavni-zpravy",
  "iDNES",              "https://servis.idnes.cz/rss.aspx?c=zpravodaj",
  "Novinky",            "https://www.novinky.cz/rss",
  "Seznam Zpravy",      "https://www.seznamzpravy.cz/rss",
  "Denik N",            "https://denikn.cz/feed/",
  "BBC World",          "http://feeds.bbci.co.uk/news/world/rss.xml",
  "The Guardian World", "https://www.theguardian.com/world/rss",
  "NYTimes World",      "https://rss.nytimes.com/services/xml/rss/nyt/World.xml",
  "NPR World",          "https://feeds.npr.org/1004/rss.xml",
  "Al Jazeera",         "https://www.aljazeera.com/xml/rss/all.xml",
  "Sky News World",     "https://feeds.skynews.com/feeds/rss/world.xml"
)

log_file <- default_log_file

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
    if (length(parts) < 2) {
      return(NULL)
    }

    group_name <- str_squish(parts[1])
    keywords_part <- paste(parts[-1], collapse = "-")
    keywords_part <- str_squish(keywords_part)

    if (!nzchar(group_name) || !nzchar(keywords_part)) {
      return(NULL)
    }

    keywords <- strsplit(keywords_part, ";", fixed = TRUE)[[1]]
    keywords <- str_squish(keywords)
    keywords <- keywords[nzchar(keywords)]

    if (length(keywords) == 0) {
      return(NULL)
    }

    tibble(
      keyword_group = group_name,
      keyword_term = keywords
    )
  }

  out <- purrr::map(raw, parse_line) %>%
    purrr::compact() %>%
    bind_rows()

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
    s <- sub(" GMT$", " +0000", s)
    s <- sub(" UTC$", " +0000", s)
    s <- sub(" BST$", " +0100", s)
    s <- sub(" CET$", " +0100", s)
    s <- sub(" CEST$", " +0200", s)
    s
  }

  parse_one <- function(s) {
    s <- normalize_tz(s)
    if (!nzchar(s)) return(as.POSIXct(NA, tz = "UTC"))

    fmts <- c(
      "%a, %d %b %Y %H:%M:%S %z",
      "%d %b %Y %H:%M:%S %z",
      "%a, %d %b %Y %H:%M %z",
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

fetch_feed <- function(name, url) {
  log_msg("INFO", paste("Loading", name))

  tryCatch({
    feed <- xml2::read_xml(url)

    items <- xml2::xml_find_all(feed, ".//*[local-name()='item']")
    if (length(items) == 0) {
      items <- xml2::xml_find_all(feed, ".//*[local-name()='entry']")
    }

    if (length(items) == 0) {
      log_msg("WARN", paste("Skipping", name, "- no items found"))
      return(tibble())
    }

    get_link_atom <- function(item) {
      href <- xml2::xml_attr(xml2::xml_find_first(item, ".//*[local-name()='link']"), "href")
      if (!is.na(href) && nzchar(href)) return(href)
      extract_item_text(item, c(".//*[local-name()='link']"))
    }

    out <- tibble(
      source = name,
      title = vapply(items, function(item) {
        extract_item_text(item, c(".//*[local-name()='title']"))
      }, character(1)),
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
      filter(!is.na(link), nzchar(link))

    bad_dates <- sum(is.na(out$datetime))
    if (bad_dates > 0) {
      log_msg("WARN", sprintf("%s: nepodarilo se naparsovat datum u %d zaznamu", name, bad_dates))
    }

    out
  }, error = function(e) {
    log_msg("WARN", paste("Skipping", name, e$message))
    tibble()
  })
}

extract_article_text <- function(link) {
  if (is.na(link) || !nzchar(link)) return("")

  out <- tryCatch({
    page <- read_html(link)

    selectors <- c(
      "article p", "main p", ".article p", ".article__content p",
      ".entry-content p", ".post-content p", ".content p", "p"
    )

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

  out
}

build_abstract <- function(summary_raw, link, max_sentences = 3, max_chars = 550) {
  out <- first_sentences(summary_raw, max_sentences = max_sentences, max_chars = max_chars)
  need_fallback <- is.na(out) | out == "" | nchar(out) < 80

  if (any(need_fallback)) {
    idx <- which(need_fallback)
    for (i in idx) {
      article_text <- extract_article_text(link[[i]])
      if (nzchar(article_text)) {
        out[[i]] <- first_sentences(article_text, max_sentences = max_sentences, max_chars = max_chars)
      }
    }
  }

  out
}

match_keywords <- function(news_df, keywords_df) {
  if (nrow(news_df) == 0) return(tibble())

  news_df %>%
    mutate(search_text = str_to_lower(paste(title, summary_raw, sep = " "))) %>%
    tidyr::crossing(keywords_df) %>%
    rowwise() %>%
    mutate(
      hit = if (!is.na(keyword_term) && nzchar(keyword_term)) {
        str_detect(search_text, regex(paste0("\\b", escape_regex(keyword_term), "\\b"), ignore_case = TRUE))
      } else FALSE
    ) %>%
    ungroup() %>%
    filter(hit) %>%
    mutate(keyword = keyword_group) %>%
    distinct(link_norm, keyword, .keep_all = TRUE)
}

read_existing_results <- function(path) {
  if (!file.exists(path)) {
    return(tibble(
      date = as.Date(character()),
      keyword = character(),
      source = character(),
      link = character(),
      abstract = character(),
      link_norm = character()
    ))
  }

  out <- tryCatch({
    df <- suppressMessages(readr::read_csv(path, show_col_types = FALSE))
    required_cols <- c("date", "keyword", "source", "link", "abstract")

    missing_cols <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0) {
      log_msg("WARN", sprintf("Existujici %s nema ocekavane sloupce, bude ignorovan.", path))
      return(tibble(
        date = as.Date(character()),
        keyword = character(),
        source = character(),
        link = character(),
        abstract = character(),
        link_norm = character()
      ))
    }

    df %>%
      transmute(
        date = as.Date(date),
        keyword = as.character(keyword),
        source = as.character(source),
        link = as.character(link),
        abstract = as.character(abstract),
        link_norm = normalize_link(link)
      )
  }, error = function(e) {
    log_msg("WARN", sprintf("Existujici %s se nepodarilo nacist, bude ignorovan: %s", path, e$message))
    tibble(
      date = as.Date(character()),
      keyword = character(),
      source = character(),
      link = character(),
      abstract = character(),
      link_norm = character()
    )
  })

  out
}

merge_results <- function(new_results, output_path) {
  existing_results <- read_existing_results(output_path)

  log_msg("INFO", sprintf("Existujici vystup obsahuje %d zaznamu", nrow(existing_results)))
  log_msg("INFO", sprintf("Novy beh vytvoril %d zaznamu", nrow(new_results)))

  combined <- bind_rows(new_results, existing_results) %>%
    mutate(
      date = as.Date(date),
      keyword = as.character(keyword),
      source = as.character(source),
      link = as.character(link),
      abstract = as.character(abstract),
      link_norm = normalize_link(link)
    ) %>%
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
log_msg("INFO", sprintf("Nacteno %d vazeb skupina->klicove slovo z %s", nrow(keywords), default_keywords_file))

all_news <- purrr::map2_dfr(rss_feeds$source, rss_feeds$url, fetch_feed)
log_msg("INFO", sprintf("Nacteno celkem %d RSS zaznamu", nrow(all_news)))

if (nrow(all_news) == 0) {
  log_msg("WARN", "Nepodarilo se nacist zadna RSS data.")
  existing_results <- read_existing_results(default_output_file) %>%
    select(date, keyword, source, link, abstract)
  readr::write_excel_csv(existing_results, default_output_file)
  quit(save = "no")
}

filtered_news <- all_news %>%
  filter(!is.na(date)) %>%
  filter(date >= date_from, date <= date_to)

log_msg("INFO", sprintf("Po filtraci na interval zustalo %d zaznamu", nrow(filtered_news)))

if (nrow(filtered_news) == 0) {
  log_msg("WARN", "V danem intervalu nebyly v RSS feedech nalezeny zadne zaznamy.")
  existing_results <- read_existing_results(default_output_file) %>%
    select(date, keyword, source, link, abstract)
  readr::write_excel_csv(existing_results, default_output_file)
  quit(save = "no")
}

results <- match_keywords(filtered_news, keywords)
log_msg("INFO", sprintf("Po matchi na klicova slova: %d zaznamu", nrow(results)))

if (nrow(results) > 0) {
  results <- results %>%
    mutate(
      abstract = build_abstract(summary_raw, link)
    ) %>%
    arrange(desc(date), keyword, source) %>%
    distinct(link_norm, keyword, .keep_all = TRUE) %>%
    transmute(
      date = as.Date(date),
      keyword = keyword,
      source = source,
      link = link,
      abstract = abstract,
      link_norm = link_norm
    )
} else {
  results <- tibble(
    date = as.Date(character()),
    keyword = character(),
    source = character(),
    link = character(),
    abstract = character(),
    link_norm = character()
  )
}

final_results <- merge_results(results, default_output_file) %>%
  select(date, keyword, source, link, abstract)

readr::write_excel_csv(final_results, default_output_file)
log_msg("INFO", sprintf("Hotovo. Ulozeno %d zaznamu do %s", nrow(final_results), default_output_file))
