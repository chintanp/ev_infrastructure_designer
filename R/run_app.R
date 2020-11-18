#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(onStart = NULL,
                    options = list(port = as.numeric(Sys.getenv("EVIDES_PORT")),
                    launch.browser = TRUE),
                    enableBookmarking = NULL,
                    
                    ...) {
  with_golem_options(
    app = auth0::shinyAppAuth0(
      ui = app_ui(),
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking
    ),
    golem_opts = list(...)
  )
}
