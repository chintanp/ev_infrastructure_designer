#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(onStart = NULL,
                    options = list(port = as.numeric(Sys.getenv("EVIDES_PORT"))), 
                    enableBookmarking = NULL,
                    ...) {
  with_golem_options(
    app = auth0::shinyAppAuth0(ui = app_ui(),
                   server = app_server,
                   onStart = onStart,
                   options = options, 
                   enableBookmarking = enableBookmarking, 
                   config_file = here::here("inst", "app", "_auth0.yml")), 
    golem_opts = list(...)
  )
}
