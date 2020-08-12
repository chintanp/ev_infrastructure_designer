#' Create the logstash format for the log
#'
#' Refer to: https://github.com/s-fleck/lgr/issues/29#issuecomment-558022635 #' for explanation
#'
#' @param event The logging event that triggers the log creation.
#' @param format_event Required for custom log format creation.
#'
LayoutLogstash <- R6::R6Class(
  "LayoutLogstash",
  inherit = lgr::LayoutJson,
  public = list(
    format_event = function(event) {
      dd <- list(
        "@message" = event$msg,
        "@fields" = c(
          level = unname(lgr::label_levels(event$level)),
          event$values[setdiff(names(event$values), c("msg", "level"))]
        )
      )
      do.call(
        jsonlite::toJSON,
        args = c(list(x = dd), get(".toJSON_args", private))
      )
    }
  )
)