#!/usr/bin/env Rscript

arg <- commandArgs(trailingOnly = TRUE)
arg <- ifelse(length(arg) == 0, "", arg)

## Fix encoding problems
# sudo locale-gen pt_BR.iso88591
# Sys.setlocale(locale = "pt_BR.utf8")

if (arg == "cron") {
  library(cronR)
  cmd <- cron_rscript("journal.R")
  cron_add(
    command = cmd,
    frequency = "daily",
    at = "00:10",
    days_of_week = 0,
    id = "journal",
    tags = "webscraping",
    description = "Collect the news"
  )
} else {
  con <- tempfile()
  sink(con, append = TRUE)

  cat("Start - Journal ", arg, as.character(Sys.time()), "\n\n")

  startTime <- Sys.time()

  library(dplyr)
  library(stringr)
  library(RMySQL)
  library(lubridate)
  devtools::load_all()
  # library(dw)

  textToUTF8 <- function(x) {
    enc <- stringi::stri_enc_detect2(x)
    enc <- sapply(enc, function(x) {
      x <- x %>%
        dplyr::filter(Confidence >= 0.3)
      if (nrow(x) == 0) {
        return("UTF-8")
      } else if ("UTF-8" %in% x$Encoding) {
        return("UTF-8")
      } else {
        x$Encoding[1]
      }
    })
    text <- mapply(text = x, enc = enc, function(text, enc) {
      iconv(text, from = enc, to = "utf8")
    }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
    return(do.call(c, text))
  }

  g1globo <- wJournal("g1globo", collect = "lastweek")
  g1globo <- g1globo %>%
    rename(ymd = date) %>%
    mutate_at(vars(title, author, metatitle, text), textToUTF8)
  con <- dbConnect(MySQL(), dbname = "dw", host = "localhost")
  dbWriteTable(con, "journalg1globo", g1globo, append = TRUE, row.names = FALSE, overwrite = FALSE)
  dbDisconnect(con)

  # folhasp <- wJournal("folhasp", collect = "lastweek")
  # folhasp <- folhasp %>%
  #   rename(ymd = date) %>%
  #   mutate_at(vars(title, text), textToUTF8)
  # con <- dbConnect(MySQL(), dbname = "dw", host = "localhost")
  # dbWriteTable(con, "journalg1globo", folhasp, append = TRUE, row.names = FALSE, overwrite = FALSE)
  # dbDisconnect(con)

  estadao <- wJournal("estadao", collect = "lastweek")
  estadao <- estadao %>%
    rename(ymd = date) %>%
    mutate_at(vars(title, text), textToUTF8)
  con <- dbConnect(MySQL(), dbname = "dw", host = "localhost")
  dbWriteTable(con, "journalg1globo", estadao, append = TRUE, row.names = FALSE, overwrite = FALSE)
  dbDisconnect(con)

  uol <- wJournal("uol", collect = "lastweek")
  uol <- uol %>%
    rename(ymd = date) %>%
    mutate_at(vars(title, text), textToUTF8)
  con <- dbConnect(MySQL(), dbname = "dw", host = "localhost")
  dbWriteTable(con, "journalg1globo", estadao, append = TRUE, row.names = FALSE, overwrite = FALSE)
  dbDisconnect(con)


  cat("g1globo: Collected ", nrow(g1globo), "\n",
      # "folhasp: Collected ", nrow(folhasp), "\n",
      "estadao: Collected ", nrow(estadao), "\n",
      "uol: Collected ", nrow(uol), "\n",

      )

  sink()
  library(telegram)
  bot <- TGBot$new(token = bot_token('LogBotAWbot'))
  bot$set_default_chat_id(Sys.getenv("chat_id"))
  s <- readLines(con)
  s <- paste0(s, "\n", collapse = "\n")
  bot$sendMessage(s)
}

