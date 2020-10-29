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
  
  sim_params <- pool %>% DBI::dbGetQuery('select 
                                         param_id, param_name, param_value, param_type, param_lower_bound, param_upper_bound
                                         from sim_params');
  
  # print(sim_params)
  global_params <- sim_params %>%
    dplyr::filter(param_type == 'global')  %>%
    dplyr::select(-param_type)

  tripgen_params <- sim_params %>%
    dplyr::filter(param_type == 'tripgen')  %>%
    dplyr::select(-param_type)

  eviabm_params <- sim_params %>%
    dplyr::filter(param_type == 'eviabm')  %>%
    dplyr::select(-param_type)
  
  stash$pool <- pool
  stash$sim_params <- sim_params
  stash$global_params <- global_params
  stash$tripgen_params <- tripgen_params
  stash$eviabm_params <- eviabm_params
  
  return (list(stash = stash))
  
}