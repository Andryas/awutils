seleniumEnv <- new.env()
assign(".port", NULL, envir = seleniumEnv)

#' Selenium Server
#'
#' @param x start or end
#' @param ... See sections
#'
#' @section start:
#' port: an integer between 4445 and 4490
#'
#' port2: an integer between 5901 and 5946
#'
#' auto: detect the ports that are not being used and start the selenium server
#' with it
#'
#' version: the version of selenium (Default: 3.141.59-zinc)
#'
#' browser: firefox or chrome
#'
#' @section end:
#' port: an integer between 4445 and 4490
#'
#' port2: an integer between 5901 and 5946
#'
#' @export
selenium <- function(x, ...) {
  if (x == "start") {
    ports <- selenium.start(...)
    Sys.sleep(30)
  } else if (x == "end") {
    ports <- selenium.end(...)
  } else {
    stop("x must be start or end")
  }
  return(ports)
}

# This function start a selenium docker
selenium.start <- function(
  port = 4445L,
  port2 = 5901L,
  auto = TRUE,
  version = "debug:3.141.59-zinc",
  browser = "firefox"
  ) {

  if (Sys.getenv("PASSWORD") == "") {
    stop(
      "You must provide your password in your ~/.Renviron\n\tEx: PASSWORD=12345"
    )
  }

  x <- paste0("echo ", Sys.getenv("PASSWORD"), " | sudo -S docker ps")
  x <- system(x, intern = TRUE)

  if (isTRUE(auto)) {
    cat("\n")
    Sys.sleep(1)

    port <- stringr::str_extract(x, "[0-9]{4}(?=->4444)")[-1]
    port2 <- stringr::str_extract(x, "[0-9]{4}(?=->5900)")[-1]
    ports <- paste0(port, port2)

    port_aux <- 4490L:4445L
    port2_aux <- 5901L:5946L
    port_aux <- port_aux[1:length(port2_aux)]

    ports_aux <- paste0(port_aux, port2_aux)
    ports <- ports_aux[which(!(ports_aux %in% ports))]
    ports <- ports[round(runif(1, 0, length(ports)))]
    port <- substr(ports, 1, 4)
    port2 <- substr(ports, 5, 8)

    cat("\t selenium ", port, " ", port2, "\n")

    invisible(
      system(
        paste0(
          "echo ",
          Sys.getenv("PASSWORD"),
          " | sudo -S docker run -d -p ", port, ":4444 -p ",
          port2, ":5900 selenium/standalone-",
          browser, "-", version
        ),
        wait = TRUE
      )
    )

    cat("\n")
    assign(".port", as.integer(c(port = port, port2 = port2)), envir = seleniumEnv)
    return(as.integer(c(port = port, port2 = port2)))
  } else {
    if (!any(stringr::str_detect(x, paste0(port, "->4444"))) &
        !any(stringr::str_detect(x, paste0(port2, "->5900")))) {

      system(
        paste0(
          "echo ",
          Sys.getenv("PASSWORD"),
          " | sudo -S docker run -d -p ", port, ":4444 -p ",
          port2, ":5900 selenium/standalone-",
          browser, "-", version
        ),
        wait = TRUE
      )

      assign(".port", as.integer(c(port = port, port2 = port2)), envir = seleniumEnv)
      return(as.integer(c(port = port, port2 = port2)))
    } else {
      message("There is already a docker in progress in these ports.")
    }
  }
}

# This function ends a selenium docker
selenium.end <- function(port, port2) {
  .port <- get(".port", envir = seleniumEnv)

  if (!is.null(.port)) {
    port <- .port[1]
    port2 <- .port[2]
  }

  if (Sys.getenv("PASSWORD") == "") {
    stop(
      "You must provide your password in your ~/.Renviron\n\tEx: PASSWORD=12345"
    )
  }

  close_selenium <- paste0("echo ", Sys.getenv("PASSWORD"), " | sudo -S docker ps")
  close_selenium <- invisible(system(close_selenium, intern = TRUE))
  close_selenium <- close_selenium[which(stringr::str_detect(close_selenium, paste0(port, "->4444", collapse = "|")))]
  close_selenium <- stringr::str_extract(close_selenium, "\\w+")
  while (TRUE) {
    pid <- invisible(system(
      paste0("ps aux | grep ", close_selenium, " | awk '{print $1 $2}'"),
      intern = TRUE
    ))
    pid <- stringr::str_extract(pid[stringr::str_detect(pid, "root")], "[0-9]+")
    if (length(pid) == 0) {
      break
    } else {
      sapply(pid, function(x) {
        capture.output(system(paste0("echo ", Sys.getenv("PASSWORD"), "| sudo -S kill ", x)))
      })
      Sys.sleep(5)
    }
  }
  capture.output(
    invisible(system(
      paste0(
        "echo ",
        Sys.getenv("PASSWORD"),
        " | sudo -S docker rm -f ",
        close_selenium
      )
    ))
  )

  assign(".port", NULL, envir = seleniumEnv)
  return("Closed")
}
