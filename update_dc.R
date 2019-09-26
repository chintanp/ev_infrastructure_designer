# This file updates the destination charger table depending on the new
# EVSEs entered

# handline commandline args...
# rem: with TRUE option below, #args[1] is the "--args" switch; skip it.
args <- commandArgs(TRUE)

# utility for checking is something is already installed, then loading.
usePackage <- function(p) {
    if (!is.element(p, installed.packages()[, 1])) {
        install.packages(p, dep = TRUE)
    }
    require(p, character.only = TRUE)
}

usePackage("dplyr")
usePackage("fuzzyjoin")
usePackage("data.table")
usePackage("RPostgres")
usePackage("DBI")
usePackage("doParallel")

a_id <- 8

main_con <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = "localhost",
    dbname = "wsdot_evse_main",
    user = Sys.getenv("MAIN_USERNAME"),
    password = Sys.getenv("MAIN_PWD")
)

query_wazip <- "select * from zipcode_record where state = 'WA'"
res_wazip <- DBI::dbSendQuery(main_con, query_wazip)
wa_zip <- DBI::dbFetch(res_wazip)
DBI::dbClearResult(res_wazip)

query_nevses <-
    paste0("select * from new_evses where analysis_id = ", a_id)
res_nevses <- DBI::dbSendQuery(main_con, query_nevses)
nevses <- DBI::dbFetch(res_nevses)
DBI::dbClearResult(res_nevses)

for (i in 1:nrow(nevses)) {
    query_dwithin <-
        paste0(
            "select zip, st_dwithin(st_setsrid(st_makepoint(w.longitude, w.latitude), 4326)::geography, st_setsrid(st_makepoint(",
            nevses$longitude[i],
            ",",
            nevses$latitude[i],
            "), 4326)::geography, 10/0.000621371) from (select * from zipcode_record where state = 'WA') as w"
        )
    idwithin <- DBI::dbGetQuery(main_con, query_dwithin)
    # Check if rows are returned from the query 
    
        idwithin$dc_chademo <- FALSE
        idwithin$dc_combo <- FALSE
        idwithin$dc_level2 <- FALSE
        # If any chademo chargers are added, then the dc_chademo is updated
        if (nevses$chademo_plug_count[i] >= 1) {
            idwithin$dc_chademo <- idwithin$st_dwithin
        }
        if (nevses$combo_plug_count[i] >= 1) {
            idwithin$dc_combo <- idwithin$st_dwithin
        }
        if (nevses$level2_plug_count[i] >= 1) {
            idwithin$dc_level2 <- idwithin$st_dwithin
        } 
        idwithin <- idwithin %>% dplyr::filter(st_dwithin == TRUE)

        
        if (nrow(idwithin) >= 1) {
            idwithin$analysis_id <- a_id
            # Once dc_chademo and dc_combo are updated, st_dwithin column can be deleted
            idwithin$st_dwithin <- NULL
            DBI::dbAppendTable(main_con, "dest_charger", idwithin, append = TRUE)
        }
    
    
}
