#!/usr/bin/env Rscript

arg <- commandArgs(trailingOnly = TRUE)
arg <- ifelse(length(arg) == 0, "", arg)

if (arg == "crontab" | arg == "cron") {
  library(cronR)
  cmd1 <- cron_rscript("superbid.R", rscript_args = c("order"))
  cron_add(
    cmd1,
    "daily",
    "7:00",
    id = "superbidOrder",
    tags = "webscraping",
    description = "Collect the order auctiones."
  )
  cmd2 <- cron_rscript("superbid.R", rscript_args = c("data"))
  cron_add(
    cmd2,
    "daily",
    "20:00",
    id = "superbidData",
    tags = "webscraping",
    description = "Collect the day's data."
  )
} else if (arg == "order" | arg == "data") {
  con <- tempfile("script.log")
  sink(con, append = TRUE)

  cat("Start - Superbid ", arg, as.character(Sys.time()), "\n\n")
  startTime <- Sys.time()

  library(dw)
  library(mongolite)

  if (arg == "order") {
    id <- wSuperbid("order", orderDate = Sys.Date() + 1, id = NULL)
    cat("Total of orders: ", length(id), "\n\n")

    m <- mongo(collection = "superbidOrder", db = "dw")
    m$insert(jsonlite::toJSON(
      list(
        "_id" = as.integer(Sys.Date()),
        timeElapsedSecs = as.numeric(difftime(Sys.time(), startTime, units = "secs")),
        type = "order",
        ids = id
      ),
      auto_unbox = TRUE
    ),
    stop_on_error = FALSE
    )
    m$disconnect()
  }

  if (arg == "data") {
    m <- mongo(collection = "superbidOrder", db = "dw")
    ids <- m$find(query = paste0('{"_id": ',  as.integer(Sys.Date()),'}'), fields = '{"ids": true}')
    ids <- ids$ids[[1]]
    m$disconnect()

    if (!is.null(ids)) {
      data <- wSuperbid("data", ids)
      cat(
        "\n",
        as.integer(Sys.Date()-1), "\t", as.character(Sys.Date()-1),
        "\n",
        "Number of ids: ", length(ids), "\n",
        "Total collected: ", length(data), "\n",
        "%: ", (length(data)/length(ids)) %>% scales::percent(), "\n\n"
      )

      data <- lapply(data, function(x) {
        names(x)[1] <- "_id"
        return(x)
      })

      m <- mongo(collection = "superbidData", db = "dw")
      out <- lapply(data, function(x) {
        m$insert(jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE), stop_on_error = FALSE)
      })
      m$disconnect()
    }
  }

  cat("End - Superbid ", arg, as.character(Sys.time()), "\n\n")
  sink()
  library(telegram)
  bot <- TGBot$new(token = bot_token('LogBotAWbot'))
  bot$set_default_chat_id(Sys.getenv("chat_id"))
  s <- readLines(con)
  s <- paste0(s, "\n", collapse = "\n")
  bot$sendMessage(s)
} else {
  print("You must use one of the follow args: cron, order or data!")
}

