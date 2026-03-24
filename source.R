#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(tidyRSS)
  library(dplyr)
  library(purrr)
  library(stringr)
  library(lubridate)
  library(readr)
  library(tibble)
  library(xml2)
  library(tidyr)
  library(rvest)
})

options(stringsAsFactors = FALSE)

# -----------------------------
# Defaults / config
# -----------------------------
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
  "Reuters World",      "https://feeds.reuters.com/Reuters/worldNews",
  "The Guardian World", "https://www.theguardian.com/world/rss",
  "AP News",            "https://apnews.com/hub/ap-top-news?output=rss",
  "NYTimes World",      "https://rss.nytimes.com/services/xml/rss/nyt/World.xml"
)

# -----------------------------
# Logging
# -----------------------------
log_file <- default_log_file
log_msg <- function(level, text) {
  line <- sprintf("[%s] [%s] %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), level, text)
  cat(line, "\n")
  cat(line, "\n", file = log_file, append = TRUE)
}

# -----------------------------
# CLI parsing
# -----------------------------
parse_args <- function(args) {
  out <- list(
    date_from = NA_character_,
    date_to = NA_character_,
    hours_back = NA_real_,
    keywords_file = default_keywords_file,
    output_file = default_output_file,
    log_file = default_log_file
  )

  i <- 1
  while (i <= length(args)) {
    a <- args[[i]]
    next_val <- function() {
      if (i == length(args)) stop(sprintf("Missing value for argument %s", a))
      args[[i + 1]]
    }

    if (a == "--date-from") out$date_from <- next_val()
    else if (a == "--date-to") out$date_to <- next_val()
    else if (a == "--hours-back") out$hours_back <- suppressWarnings(as.numeric(next_val()))
    else if (a == "--keywords-file") out$keywords_file <- next_val()
    else if (a == "--output-file") out$output_file <- next_val()
    else if (a == "--log-file") out$log_file <- next_val()
    else if (a %in% c("-h", "--help")) {
      cat(
"Usage:
  Rscript news_monitor_v2.R
  Rscript news_monitor_v2.R --date-from 2026-03-20 --date-to 2026-03-24
  Rscript news_monitor_v2.R --hours-back 12

Options:
  --date-from YYYY-MM-DD   Start date (inclusive)
  --date-to   YYYY-MM-DD   End date (inclusive)
  --hours-back N           Rolling lookback window for automation
  --keywords-file FILE     Default: keywords.txt
  --output-file FILE       Default: source.csv
  --log-file FILE          Default: news_monitor.log
"
      )
      quit(save = "no", status = 0)
    }
    if (a %in% c("--date-from", "--date-to", "--hours-back", "--keywords-file", "--output-file", "--log-file")) i <- i + 1
    i <- i + 1
  }
  out
}

prompt_date <- function(label) {
  repeat {
    x <- readline(sprintf("%s (YYYY-MM-DD): ", label))
    d <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
    if (!is.na(d)) return(d)
    cat("Neplatne datum. Zadej prosim datum ve formatu YYYY-MM-DD.\n")
  }
}

resolve_interval <- function(parsed_args) {
  if (!is.na(parsed_args$hours_back)) {
    if (parsed_args$hours_back <= 0) stop("--hours-back musi byt kladne cislo.")
    dt_to <- Sys.time()
    dt_from <- dt_to - hours(parsed_args$hours_back)
    return(list(
      mode = "rolling",
      datetime_from = dt_from,
      datetime_to = dt_to,
      date_from = as.Date(dt_from),
      date_to = as.Date(dt_to)
    ))
  }

  if (!is.na(parsed_args$date_from) && !is.na(parsed_args$date_to)) {
    d1 <- as.Date(parsed_args$date_from)
    d2 <- as.Date(parsed_args$date_to)
    if (is.na(d1) || is.na(d2)) stop("--date-from a --date-to musi byt ve formatu YYYY-MM-DD.")
    if (d2 < d1) stop("Koncove datum nesmi byt mensi nez pocatecni datum.")
    return(list(
      mode = "date_range",
      datetime_from = as.POSIXct(d1),
      datetime_to = as.POSIXct(d2) + hours(23) + minutes(59) + seconds(59),
      date_from = d1,
      date_to = d2
    ))
  }

  cat("=====================================\n")
  cat(" News monitoring z RSS zdroju\n")
  cat("=====================================\n\n")
  d1 <- prompt_date("Zadej pocatecni datum")
  d2 <- prompt_date("Zadej koncove datum")
  if (d2 < d1) stop("Koncove datum nesmi byt mensi nez pocatecni datum.")

  list(
    mode = "date_range",
    datetime_from = as.POSIXct(d1),
    datetime_to = as.POSIXct(d2) + hours(23) + minutes(59) + seconds(59),
    date_from = d1,
    date_to = d2
  )
}

# -----------------------------
# Helpers
# -----------------------------
escape_regex <- function(x) {
  stringr::str_replace_all(x, "([][{}()+*^$|\\?.])", "\\\\\\1")
}

coalesce_cols <- function(df, candidates, default = NA_character_) {
  existing <- intersect(candidates, names(df))
  if (length(existing) == 0) return(rep(default, nrow(df)))
  out <- df[[existing[1]]]
  if (length(existing) > 1) {
    for (nm in existing[-1]) out <- dplyr::coalesce(out, df[[nm]])
  }
  out
}

strip_html <- function(x) {
  x <- ifelse(is.na(x), "", x)
  vapply(
    x,
    function(one) {
      one <- trimws(one)
      if (identical(one, "")) return("")
      cleaned <- tryCatch(
        xml2::xml_text(xml2::read_html(paste0("<div>", one, "</div>"))),
        error = function(e) one
      )
      stringr::str_squish(cleaned)
    },
    character(1)
  )
}

normalize_link <- function(x) {
  x <- trimws(x)
  x <- str_replace(x, "#.*$", "")
  x <- str_replace(x, "\\?utm_[^=]+=[^&]+(&|$)", "?")
  x <- str_replace_all(x, "\\?utm_[^=]+=[^&]+(&|$)", "?")
  x <- str_replace(x, "[?&]$", "")
  x <- str_replace(x, "\\?$", "")
  x <- str_replace(x, "/$", "")
  x
}

first_sentences <- function(x, max_sentences = 3, max_chars = 550) {
  x <- strip_html(x)
  vapply(
    x,
    function(one) {
      if (is.na(one) || one == "") return("")
      parts <- unlist(str_split(one, "(?<=[.!?])\\s+", perl = TRUE))
      parts <- parts[nzchar(parts)]
      abstract <- if (length(parts) == 0) one else paste(head(parts, max_sentences), collapse = " ")
      abstract <- str_squish(abstract)
      if (nchar(abstract) > max_chars) abstract <- paste0(substr(abstract, 1, max_chars - 3), "...")
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

  parsed <- str_split_fixed(raw, ";", 2)
  out <- tibble(
    keyword_cz = str_squish(parsed[, 1]),
    keyword_en = str_squish(parsed[, 2])
  ) %>%
    mutate(
      keyword_cz = na_if(keyword_cz, ""),
      keyword_en = na_if(keyword_en, "")
    ) %>%
    filter(!(is.na(keyword_cz) & is.na(keyword_en)))

  if (nrow(out) == 0) stop("V keywords.txt nebyly nalezeny validni dvojice 'cz;en'.")
  out
}

parse_pub_datetime <- function(x) {
  suppressWarnings(lubridate::parse_date_time(
    x,
    orders = c(
      "Ymd HMS z", "Ymd HMS",
      "a, d b Y H:M:S z", "d b Y H:M:S z",
      "a, d b Y H:M z",   "d b Y H:M z",
      "Y-m-dTH:M:S z",    "Y-m-dTH:M:S",
      "Y-m-d H:M:S",      "Y-m-d"
    ),
    locale = "C"
  ))
}

extract_article_text <- function(link) {
  if (is.na(link) || !nzchar(link)) return("")
  txt <- tryCatch({
    page <- read_html(link)
    candidates <- c(
      "article p",
      "main p",
      ".article p",
      ".article__content p",
      ".post-content p",
      ".entry-content p",
      ".content p",
      "p"
    )

    for (css in candidates) {
      nodes <- html_elements(page, css)
      if (length(nodes) == 0) next
      text <- html_text2(nodes)
      text <- stringr::str_squish(text)
      text <- text[nchar(text) > 40]
      text <- text[!str_detect(text, "^(Copyright|All rights reserved|Subscribe|Sign up|Read more)")]
      if (length(text) >= 3) {
        return(paste(text, collapse = " "))
      }
    }
    ""
  }, error = function(e) {
    log_msg("WARN", sprintf("Nepodarilo se nacist clanek %s (%s)", link, e$message))
    ""
  })
  txt
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

fetch_feed <- function(feed_name, feed_url) {
  log_msg("INFO", sprintf("Nacitam feed: %s", feed_name))
  out <- tryCatch({
    df <- tidyRSS::tidyfeed(feed_url)
    if (!is.data.frame(df) || nrow(df) == 0) return(tibble())

    tibble(
      source = feed_name,
      title = coalesce_cols(df, c("item_title", "title")),
      link = coalesce_cols(df, c("item_link", "link", "feed_link")),
      summary_raw = coalesce_cols(df, c("item_description", "description", "item_summary", "summary", "content")),
      pub_raw = coalesce_cols(df, c("item_pub_date", "pub_date", "published", "updated", "dc_date"))
    ) %>%
      mutate(
        title = str_squish(strip_html(title)),
        link = str_trim(link),
        link_norm = normalize_link(link),
        summary_raw = str_squish(summary_raw),
        datetime = parse_pub_datetime(pub_raw),
        date = as.Date(datetime)
      ) %>%
      filter(!is.na(link), nzchar(link))
  }, error = function(e) {
    log_msg("WARN", sprintf("Preskakuji %s: %s", feed_name, e$message))
    tibble()
  })
  out
}

match_keywords <- function(news_df, keywords_df) {
  if (nrow(news_df) == 0) return(tibble())

  news_df %>%
    mutate(search_text = str_to_lower(paste(title, strip_html(summary_raw), sep = " "))) %>%
    tidyr::crossing(keywords_df) %>%
    mutate(
      hit_cz = ifelse(!is.na(keyword_cz), str_detect(search_text, regex(paste0("\\b", escape_regex(keyword_cz), "\\b"), ignore_case = TRUE)), FALSE),
      hit_en = ifelse(!is.na(keyword_en), str_detect(search_text, regex(paste0("\\b", escape_regex(keyword_en), "\\b"), ignore_case = TRUE)), FALSE)
    ) %>%
    filter(hit_cz | hit_en) %>%
    mutate(
      keyword = case_when(
        hit_cz & !is.na(keyword_cz) ~ keyword_cz,
        hit_en & !is.na(keyword_en) ~ keyword_en,
        TRUE ~ coalesce(keyword_cz, keyword_en)
      )
    ) %>%
    distinct(link_norm, keyword, .keep_all = TRUE)
}

# -----------------------------
# Main
# -----------------------------
args <- parse_args(commandArgs(trailingOnly = TRUE))
log_file <- args$log_file
cat("", file = log_file, append = TRUE)

interval <- resolve_interval(args)
log_msg("INFO", sprintf("Interval: %s -> %s (mode=%s)", interval$datetime_from, interval$datetime_to, interval$mode))

keywords <- read_keywords(args$keywords_file)
log_msg("INFO", sprintf("Nacteno %d dvojic klicovych slov z %s", nrow(keywords), args$keywords_file))

all_news <- purrr::map2_dfr(rss_feeds$source, rss_feeds$url, fetch_feed)

if (nrow(all_news) == 0) {
  log_msg("WARN", "Nepodarilo se nacist zadna RSS data.")
  readr::write_excel_csv(tibble(
    date = as.Date(character()),
    keyword = character(),
    source = character(),
    link = character(),
    abstract = character()
  ), args$output_file)
  quit(save = "no")
}

filtered_news <- all_news %>%
  filter(!is.na(datetime)) %>%
  filter(datetime >= interval$datetime_from, datetime <= interval$datetime_to)

log_msg("INFO", sprintf("Po filtraci na interval zustalo %d zaznamu", nrow(filtered_news)))

if (nrow(filtered_news) == 0) {
  log_msg("WARN", "V danem intervalu nebyly v RSS feedech nalezeny zadne zaznamy.")
  readr::write_excel_csv(tibble(
    date = as.Date(character()),
    keyword = character(),
    source = character(),
    link = character(),
    abstract = character()
  ), args$output_file)
  quit(save = "no")
}

results <- match_keywords(filtered_news, keywords)
log_msg("INFO", sprintf("Po matchi na klicova slova: %d zaznamu", nrow(results)))

if (nrow(results) > 0) {
  results <- results %>%
    mutate(abstract = build_abstract(summary_raw, link)) %>%
    arrange(desc(date), keyword, source) %>%
    distinct(link_norm, keyword, .keep_all = TRUE) %>%
    transmute(
      date = as.Date(date),
      keyword = keyword,
      source = source,
      link = link,
      abstract = abstract
    )
} else {
  results <- tibble(
    date = as.Date(character()),
    keyword = character(),
    source = character(),
    link = character(),
    abstract = character()
  )
}

readr::write_excel_csv(results, args$output_file)
log_msg("INFO", sprintf("Hotovo. Ulozeno %d zaznamu do %s", nrow(results), args$output_file))
