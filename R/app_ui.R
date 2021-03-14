#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  new_submit_tab <- bs4Dash::bs4TabItem(tabName = "new_submit_tab",
                                        fluidRow(
                                          column(width = 9,
                                                 mod_mapdes_ui("mapdes_ui_1")),
                                          column(width = 3,
                                                 mod_config_ui("config_ui_1"))
                                        ))
  
  parameters_tab <- bs4Dash::bs4TabItem(tabName = "parameters_tab",
                                        fluidRow(
                                          column(width = 4,
                                                 mod_globals_params_ui("globals_params_ui_1")),
                                          column(width = 4,
                                                 mod_tripgen_params_ui("tripgen_params_ui_1")),
                                          column(width = 4,
                                                 mod_eviabm_params_ui("eviabm_params_ui_1"))
                                        ))
  
  cs_upload_tab <-  bs4Dash::bs4TabItem(tabName = "cs_upload_tab", mod_upload_cs_ui("upload_cs_ui_1"))
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Reload of the page works now - as per the hack suggested here: https://github.com/curso-r/auth0/issues/54#issuecomment-558977120
    tags$script(
      htmlwidgets::JS(
        "setTimeout(function(){history.pushState({}, 'Page Title', window.location.pathname);},2000);"
      )
    ),
    # List the first level UI elements here
    bs4Dash::bs4DashPage(
      enable_preloader = FALSE,
      navbar = bs4Dash::bs4DashNavbar(
        skin = "light",
        status = "white",
        border = TRUE,
        sidebarIcon = "bars",
        controlbarIcon = "th",
        fixed = FALSE,
        HTML("<b> EV Infrastructure Designer </b>")
      ),
      sidebar = bs4Dash::bs4DashSidebar(
        skin = "light",
        status = "primary",
        title = "EVIDES",
        brandColor = "white",
        url = "",
        src = "https://i.imgur.com/WY1VkRu.png",
        elevation = 4,
        opacity = 2.2,
        bs4Dash::bs4SidebarMenu(
          bs4Dash::bs4SidebarHeader(""),
          bs4Dash::bs4SidebarMenuItem("New Submission",
                                      tabName = "new_submit_tab",
                                      icon = "plus-circle"),
          bs4Dash::bs4SidebarMenuItem("File Upload",
                                      tabName = "cs_upload_tab",
                                      icon = "arrow-up"),
          bs4Dash::bs4SidebarMenuItem("Parameters",
                                      tabName = "parameters_tab",
                                      icon = "sliders-h")
        )
      ),
      controlbar = bs4Dash::bs4DashControlbar(skin = "light", tags$div(id = "userInfo")),
      footer = bs4Dash::bs4DashFooter(
        copyrights = a(
          href = "https://faculty.washington.edu/dwhm/",
          target = "_blank",
          "Chintan Pathak and Don MacKenzie, UW"
        ),
        right_text = "2020"
      ),
      title = "test",
      body = bs4Dash::bs4DashBody(
        shinyjs::useShinyjs(),
        # fresh::use_theme(bgtheme),
        bs4Dash::bs4TabItems(new_submit_tab,
                             cs_upload_tab,
                             parameters_tab)
        
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path('www', app_sys('app/www'))
  
  tags$head(favicon(),
            bundle_resources(path = app_sys('app/www'),
                             app_title = 'evides'))
  # Add here other external resources
  # for example, you can add shinyalert::useShinyalert() )
}
