#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import shinycssloaders
#' @import shinydashboardPlus
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    ui = shinydashboard::dashboardPage(
      skin = "yellow",
      header = shinydashboard::dashboardHeader(title = "When should I start my dishwasher?",
                                               titleWidth = 400),
      sidebar = shinydashboard::dashboardSidebar(disable = TRUE),
      body = shinydashboard::dashboardBody(fluidRow(
        column(width = 3,
               box(
                 width = NULL,
                 box(
                   width = NULL,
                   status = "warning",
                   title = span(icon("calendar"), "Date options"),
                   uiOutput("daterange")
                 ),
                 box(
                   width = NULL,
                   status = "danger",
                   title = span(icon("soap"), "Dishwasher options"),
                   uiOutput("usage_time"),
                   radioButtons(
                     "limit_search",
                     "Limit best time search to next hour and onwards",
                     c("Yes" = TRUE,
                       "No" = FALSE),
                     selected = TRUE
                   )
                 )
               )),
        column(
          width = 9,
          box(
            width = NULL,
            title = "Electricity price per hour",
            girafeOutput("plot_price",
                         width = "100%") %>%
              withSpinner(type = 6)
          )
        )
      ))
    ),
    shinydashboardPlus::dashboardFooter(
      left = HTML(
        'By Kristjan Pullerits</br><a href="https://github.com/kpullerits/electricity-price-app">https://github.com/kpullerits/electricity-price-app</a>'
      ),
      right = HTML(
        '<p><a href="https://www.elprisetjustnu.se"><img src="https://ik.imagekit.io/ajdfkwyt/hva-koster-strommen/elpriser-tillhandahalls-av-elprisetjustnu_ttNExOIU_.png" alt="Elpriser tillhandahålls av Elpriset just nu.se" width="200" height="45"></a></p>'
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
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "electricity.price.app"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
