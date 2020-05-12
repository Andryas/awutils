#!/usr/bin/env Rscript

library(dplyr)
library(stringr)
library(RMySQL)
library(lubridate)
library(jsonlite)
library(dw)

arg <- commandArgs(trailingOnly = TRUE)
arg <- if(length(arg) == 0) stop("A url must be provide") else arg

arg <- "https://play.google.com/store/apps/details?id=com.splendapps.splendo&hl=pt_BR"
id <- stringr::str_extract(arg, "(?<=\\?id=).+\\b")
id <- stringr::str_replace_all(id, "[._-]+", "0")

tb <- wPlayStoreReview(arg, N = 5000)

tb <- tb %>%
  mutate(stars = str_extract(stars, "(?<=com )[0-9](?= de)"),
         stars = as.integer(stars),
         date = dmy(date),
         likes = as.integer(likes),
         idapp = id) %>%
  distinct() %>%
  select(idapp, everything()) %>%
  rename(ymd = date)

con <- dbConnect(MySQL(), dbname = "dw", host = "localhost")
dbWriteTable(con, "playstore", tb, append = TRUE, row.names = FALSE, overwrite = FALSE)
dbDisconnect(con)
