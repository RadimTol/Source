#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(tidyRSS)
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

# -----------------------------
# CONFIG
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
# LOGGING
# -----------------------------
log_file <- default_log_file
log_msg <- function(level, text) {
  line <- sprintf("[%s] [%s] %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), level, text)
  cat(line, "\n")
  cat(line, "\n", file = log_file, append = TRUE)
}

# -----------------------------
# DATETIME PARSER (FIX)
# -----------------------------
parse_pub_datetime <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""

  parse_one <- function(s) {
    s <- trimws(s)
    if (!nzchar(s)) return(as.POSIXct(NA, tz = "UTC"))

    formats <- c(
      "%a, %d %b %Y %H:%M:%S %z",
      "%d %b %Y %H:%M:%S %z",
      "%Y-%m-%dT%H:%M:%S%z",
      "%Y-%m-%dT%H:%M:%SZ",
      "%Y-%m-%d %H:%M:%S",
      "%Y-%m-%d"
    )

    for (fmt in formats) {
      dt <- suppressWarnings(as.POSIXct(strptime(s, fmt, tz = "UTC")))
      if (!is.na(dt)) return(dt)
    }

    as.POSIXct(NA, tz = "UTC")
  }

  as.POSIXct(vapply(x, parse_one, as.POSIXct(NA, tz = "UTC")),
             origin = "1970-01-01", tz = "UTC")
}

# -----------------------------
# HELPERS
# -----------------------------
strip_html <- function(x) {
  x <- ifelse(is.na(x), "", x)
  vapply(x, function(one) {
    one <- trimws(one)
    if (one == "") return("")
    tryCatch(xml_text(read_html(paste0("<div>", one, "</div>"))),
             error = function(e) one)
  }, character(1))
}

normalize_link <- function(x) {
  x <- trimws(x)
  x <- str_replace(x, "#.*$", "")
  x <- str_replace(x, "\\?utm_[^=]+=[^&]+(&|$)", "?")
  x <- str_replace(x, "[?&]$", "")
  x <- str_replace(x, "\\?$", "")
  x <- str_replace(x, "/$", "")
  x
}

first_sentences <- function(x) {
  parts <- str_split(strip_html(x), "(?<=[.!?])\\s+", perl = TRUE)
  sapply(parts, function(p) paste(head(p, 3), collapse = " "))
}

# -----------------------------
# FETCH FEED (FIX)
# -----------------------------
fetch_feed <- function(name, url) {
  log_msg("INFO", paste("Loading", name))

  tryCatch({
    df <- tidyfeed(url)

    tmp <- tibble(
      source = name,
      title = df$item_title,
      link = df$item_link,
      summary = df$item_description,
      pub = df$item_pub_date
    )

    tmp$datetime <- parse_pub_datetime(tmp$pub)
    tmp$date <- as.Date(tmp$datetime)
    tmp$link_norm <- normalize_link(tmp$link)

    tmp
  }, error = function(e) {
    log_msg("WARN", paste("Skipping", name, e$message))
    tibble()
  })
}

# -----------------------------
# MAIN
# -----------------------------
args <- commandArgs(trailingOnly = TRUE)

date_from <- ifelse(length(args) >= 1, as.Date(args[1]), Sys.Date() - 1)
date_to   <- ifelse(length(args) >= 2, as.Date(args[2]), Sys.Date())

log_msg("INFO", paste("Interval:", date_from, "->", date_to))

keywords <- readLines("keywords.txt")

news <- map2_dfr(rss_feeds$source, rss_feeds$url, fetch_feed)

news <- news %>%
  filter(!is.na(datetime)) %>%
  filter(date >= date_from & date <= date_to)

log_msg("INFO", paste("After filter:", nrow(news)))

if (nrow(news) == 0) {
  write_csv(tibble(), default_output_file)
  quit()
}

news$text <- tolower(paste(news$title, news$summary))

results <- news %>%
  rowwise() %>%
  mutate(keyword = paste(keywords[str_detect(text, keywords)], collapse = ";")) %>%
  filter(keyword != "") %>%
  mutate(abstract = first_sentences(summary)) %>%
  select(date, keyword, source, link, abstract)

write_csv(results, default_output_file)

log_msg("INFO", paste("Saved:", nrow(results)))
