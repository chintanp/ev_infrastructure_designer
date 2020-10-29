# Set options here
options(golem.app.prod = FALSE, auth0_config_file = here::here('inst', 'app', '_auth0.yml')) # TRUE = production mode, FALSE = development mode

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# Run the application
run_app()
