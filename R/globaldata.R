GlobalModule <- function(input, output, session) {
  stash = reactiveValues()

  # Setup logging -----------------

  # source('R/setup_logging.R')
  # pkgload::load_all("./")
  
  lg <-
    lgr::get_logger("test")$set_propagate(FALSE)$set_appenders(lgr::AppenderJson$new(layout = LayoutLogstash$new(), file = here::here(
      paste0("logs/evides.log")
    )))
  

  # Database connection ---------------

  if (!DBI::dbCanConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("MAIN_HOST"),
    dbname = Sys.getenv("MAIN_DB"),
    user = Sys.getenv("MAIN_USER"),
    password = Sys.getenv("MAIN_PWD"),
    port = Sys.getenv("MAIN_PORT")
  )) {
    lg$log(level = "fatal",
           msg = "Cannot connect to database",
           "ip" = ipify::get_ip())
    # Exit if DB cannot connect
    stop("Cannot connect to database")
  }
  
  pool <- pool::dbPool(
    drv = RPostgres::Postgres(),
    dbname = Sys.getenv("MAIN_DB"),
    host = Sys.getenv("MAIN_HOST"),
    user = Sys.getenv("MAIN_USER"),
    password = Sys.getenv("MAIN_PWD"),
    port = Sys.getenv("MAIN_PORT")
  )
  stash$pool <- pool
  
  return (list(stash = stash))
  
}