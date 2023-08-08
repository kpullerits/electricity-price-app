#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import httr
#' @import jsonlite
#' @import tidyr
#' @import stringr
#' @import ggplot2
#' @import ggiraph
#' @import dplyr
#' @import data.table
#' @import viridis
#' @import zoo
#' @import lubridate
#' @import wesanderson
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  system_time <- reactive({
    lubridate::with_tz(Sys.time(),
                       tzone = "Europe/Stockholm")
  })

  output$daterange <- renderUI({
    req(system_time())
    req(input$electricity_zone)

    start_val <- lubridate::date(system_time())
    end_val <- lubridate::date(system_time())
    hour_of_day <- as.numeric(format(as.POSIXct(system_time()), format = "%H"))

    date_string <- as.character(end_val + 1) %>%
      str_split(., "-")
    year = date_string[[1]][1]
    month = date_string[[1]][2]
    day = date_string[[1]][3]
    if(
      hour_of_day >= 13 &
      httr::GET(
        paste0(
          "https://www.elprisetjustnu.se/api/v1/prices/",
          year,
          "/",
          month,
          "-",
          day,
          "_",
          input$electricity_zone,
          ".json"
        )
      )$status_code == 200){
      end_val <- end_val + 1
    }
    dateRangeInput("daterange",
                   label = span(icon("calendar"),"Date range"),
                   start = start_val,
                   end = end_val,
                   min = "2022-11-01",
                   max = end_val,
                   weekstart = 1)
  })
  max_val_usage_time <- reactive({
    req(input$daterange)
    req(df_price())
    req(input$limit_search)

    df <- df_price()
    if(input$limit_search){
      df <- df %>%
        filter(time_start > system_time())
    }

    max_time <- as.integer(difftime(max(df$time_start), min(df$time_start), units="hours")) + 1
    return(max_time)
  })

  output$usage_time <- renderUI({
    req(input$daterange)
    req(df_price())
    req(input$limit_search)
    req(max_val_usage_time())

    numericInput(
      "usage_time",
      label = span(icon("stopwatch"), "Dishwasher run time (hours)"),
      value = 2,
      min = 1,
      max = max_val_usage_time(),
      step = 1
    )
  })

  observe({
    req(input$usage_time)
    req(max_val_usage_time())
    if (input$usage_time > max_val_usage_time()) {
      showModal(
        modalDialog(
          title = strong("Warning!",
                         style = "font-size:24px;
                                  color: red;"),
          p(paste0("Dishwasher run time exceeds the limits (",
                   max_val_usage_time(),
                   " hours). Setting to default (2 hours)"
                   ),
            style = "font-size:16px"),
          footer = modalButton("Close"),
          easyClose = TRUE
        ))
      updateNumericInput(session, "usage_time", value = 2)
      return()
    }
  })

  df_price <- reactive({
    req(input$daterange)
    req(input$electricity_zone)
    req(system_time())

    if(input$daterange[1] > input$daterange[2]){
      showModal(
        modalDialog(
          title = strong("Warning!",
                         style = "font-size:24px;
                                  color: red;"),
          p("First date needs to be earlier than second.",
          style = "font-size:16px"),
          footer = modalButton("Close"),
          easyClose = TRUE
        )
      )
      return()
    }
    dates <- seq(input$daterange[1], input$daterange[2], by="days") %>%
      as.character()

    df_list <- list()
    i = 1
    for (date in dates) {
      date_string <- as.character(date) %>%
        str_split(., "-")
      year = date_string[[1]][1]
      month = date_string[[1]][2]
      day = date_string[[1]][3]

      res = httr::GET(
        paste0(
          "https://www.elprisetjustnu.se/api/v1/prices/",
          year,
          "/",
          month,
          "-",
          day,
          "_",
          input$electricity_zone,
          ".json"
        )
      )
      data <- rawToChar(res$content)
      df_list[[i]] <- as_tibble(jsonlite::fromJSON(data))
      i <- i + 1
    }
    out_df <- rbindlist(df_list)
    out_df <- out_df %>%
      mutate(time_start_raw = time_start,
             time_end_raw = time_end) %>%
      mutate(time_start = lubridate::as_datetime(time_start,
                                      tz = "Europe/Stockholm"),
             time_end = lubridate::as_datetime(time_end,
                                    tz = "Europe/Stockholm")) %>%
      mutate(
        time_start_year = lubridate::year(time_start),
        time_start_month = lubridate::month(time_start),
        time_start_day = lubridate::mday(time_start)
      ) %>%
      mutate(time_now = system_time()) %>%
      mutate(time_now_floored = floor_date(time_now, unit = "hour")) %>%
      mutate(min_last_h = lubridate::minute(time_now)) %>%
      mutate(min_curr_h = 60 - min_last_h) %>%
      mutate(SEK_per_kwh_last_h = SEK_per_kWh * min_last_h / 60,
             SEK_per_kwh_curr_h = SEK_per_kWh * min_curr_h / 60) %>%
      mutate(time_start_from_time_now = time_start + minutes(min_last_h)) %>%
      mutate(hours_until_start = difftime(time_start_from_time_now, floor_date(time_now, unit = "minute"), units = "hours"))

    return(out_df)
  })

  output$plot_price <- renderGirafe({
    req(df_price())
    req(input$usage_time)
    req(df_best_time())
    req(df_best_time_from_now())

    df_for_plot <- df_price() %>%
      mutate(tooltip = paste0(
        "From ", time_start, " to ", time_end, "<br/>",
        "Price: ", signif(x = SEK_per_kWh, digits = 3), " SEK / kWh", "<br/>")
      ) %>%
      mutate(time_start = time_start)

    mean_line <- df_for_plot %>%
      summarize(mean_SEK_per_kWh = mean(SEK_per_kWh, na.rm = TRUE)) %>%
      mutate(tooltip = paste0("Mean SEK/kWh: ", signif(mean_SEK_per_kWh, digits = 3)))

    first_best_time <- df_best_time() %>%
      slice(1) %>%
      mutate(time_end_best_time = time_start + hours(input$usage_time))

    groups_in_df <- df_for_plot %>%
      distinct(time_start_year, time_start_month, time_start_day) %>%
      nrow()

    plot <- df_for_plot %>%
      ggplot(aes(
      )) +
      annotate(
        "rect",
        xmin = first_best_time$time_start,
        xmax = first_best_time$time_end_best_time,
        ymin = -Inf,
        ymax =  Inf,
        x = NULL,
        y = NULL,
        fill = "mediumseagreen",
        col = "black",
        alpha = 0.75
      ) +
      annotate(
        "rect",
        xmin = df_best_time_from_now()[1]$time_start_from_time_now,
        xmax = df_best_time_from_now()[1]$time_end_from_time_now,
        ymin = -Inf,
        ymax =  Inf,
        x = NULL,
        y = NULL,
        fill = "orange",
        col = "black",
        alpha = 0.75
      ) +
      geom_rect_interactive(
        aes(
          fill = factor(paste(
            time_start_year, time_start_month, time_start_day
          )),
          xmin = time_start,
          xmax = time_end,
          ymin = 0,
          ymax = SEK_per_kWh,
          data_id = time_start,
          tooltip = tooltip
        ),
        show.legend = FALSE
      )+
      geom_hline_interactive(
        data = mean_line,
        color = "gray10",
        size = 1,
        aes(
          yintercept = mean_SEK_per_kWh,
          data_id = mean_SEK_per_kWh,
          tooltip = paste0("Mean SEK/kWh: ", signif(mean_SEK_per_kWh, digits = 3))
        )
      ) +
      geom_hline_interactive(
        data = first_best_time,
        color = "mediumseagreen",
        size = 1,
        aes(
          yintercept = rolling_mean_SEK_per_kWh,
          data_id = rolling_mean_SEK_per_kWh,
          tooltip = paste0("Mean SEK/kWh: ", signif(rolling_mean_SEK_per_kWh, digits = 3))
        )
      ) +
      geom_hline_interactive(
        data = df_best_time_from_now()[1],
        color = "orange",
        size = 1,
        aes(
          yintercept = SEK_per_kWh_usage_time,
          data_id = SEK_per_kWh_usage_time,
          tooltip = paste0("Mean SEK/kWh: ", signif(SEK_per_kWh_usage_time, digits = 3))
        )
      ) +
      annotate(
        "text",
        label = paste0("Best time to start your\ndevice: ", first_best_time$time_start),
        x = first_best_time$time_end_best_time + minutes(30),
        y = Inf,
        size = 4,
        hjust = 0,
        vjust = 1,
        col = "mediumseagreen"
      ) +
      annotate(
        "text",
        label = paste0("Mean price: ", signif(mean_line$mean_SEK_per_kWh, digits = 3), " SEK/kWh"),
        x =  min(df_for_plot$time_start),
        y = mean_line$mean_SEK_per_kWh,
        size = 4,
        hjust = 0,
        vjust = 1,
        col = "gray10"
      ) +
      annotate(
        "text",
        label = paste0("Mean price, best time: ", signif(first_best_time$rolling_mean_SEK_per_kWh, digits = 3), " SEK/kWh"),
        x =  first_best_time$time_end_best_time + minutes(30),
        y = first_best_time$rolling_mean_SEK_per_kWh,
        size = 4,
        hjust = 0,
        vjust = 1,
        col = "mediumseagreen"
      ) +
      annotate(
        "text",
        label = paste0("Mean price, best time, ", df_best_time_from_now()[1]$hours_until_start,
                       "h from now: "
                       ,signif(df_best_time_from_now()[1]$SEK_per_kWh_usage_time , digits = 3), " SEK/kWh"),
        x =  df_best_time_from_now()[1]$time_start_from_time_now + minutes(30),
        y = df_best_time_from_now()[1]$SEK_per_kWh_usage_time,
        size = 4,
        hjust = 0,
        vjust = 1,
        col = "orange"
      ) +
      theme_classic()+
      labs(
        y = "SEK/kWh",
        x = "Date"
      )+
      scale_fill_manual(values = wes_palette("Moonrise3", n = groups_in_df, type = "continuous"))

    if(system_time() > min(df_for_plot$time_start) &
       system_time() < max(df_for_plot$time_start)){
      plot <- plot +
        geom_vline_interactive(
          color = "firebrick4",
          size = 1,

          aes(
            xintercept = system_time(),
            tooltip = paste0("App start time: ", system_time())
          )
        )+
        annotate(
          "text",
          label = paste0("App start time: ", format(system_time(), "%H:%M")),
          x =  system_time(),
          y = Inf,
          size = 4,
          hjust = 1,
          vjust = 1,
          col = "firebrick4"
        )
    }
    ggiraph::girafe(
      ggobj = plot,
      width_svg = 9,
      options = list(
        ggiraph::opts_hover_inv(css = "opacity:0.6;"),
        ggiraph::opts_selection(type = "none")
      )
    )
  })

  df_best_time <- reactive({
    req(input$daterange)
    req(df_price())
    req(input$usage_time)
    req(input$limit_search)
    req(system_time())

    df_best_time_out <- df_price()
    if(input$limit_search){
      df_best_time_out <- df_best_time_out %>%
        filter(time_start > system_time())
    }
    df_best_time_out <- df_best_time_out %>%
      mutate(rolling_mean_SEK_per_kWh = rollmean(SEK_per_kWh,
                                                 input$usage_time,
                                                 na.pad=TRUE,
                                                 align="left"))
    df_best_time_out <- df_best_time_out %>%
      arrange(rolling_mean_SEK_per_kWh)
  })

  df_best_time_from_now <- reactive({
    req(input$daterange)
    req(df_price())
    req(input$usage_time)
    req(input$limit_search)
    req(system_time())

    usage_time = input$usage_time
    df_best_time_out <- df_price() %>%
      mutate(time_end_from_time_now = time_start_from_time_now + usage_time)

    if(input$limit_search){
      df_best_time_out <- df_best_time_out %>%
        filter(time_start >= time_now_floored)
    }

    best_time_list <- list()
    for (i in 1:(nrow(df_best_time_out) - usage_time)) {
      df_loop <- df_best_time_out %>%
        slice(i: (i + usage_time))
      SEK_per_kWh_vctr <- df_loop %>%
        pull(SEK_per_kWh)

      sum_SEK_per_kWh <- sum(c(first(df_loop$SEK_per_kwh_curr_h), SEK_per_kWh_vctr[-c(1, length(SEK_per_kWh_vctr))], last(df_loop$SEK_per_kwh_last_h)))
      SEK_per_kWh_usage_time <- sum_SEK_per_kWh / (usage_time)

      best_time_list[[i]] <- tibble(
        SEK_per_kWh_usage_time = SEK_per_kWh_usage_time,
        time_start_from_time_now = first(df_loop$time_start_from_time_now),
        hours_until_start = first(df_loop$hours_until_start),
        time_end_from_time_now = last(df_loop$time_end_from_time_now)
      )

    }

    best_time_df <- data.table::rbindlist(best_time_list)
    df_best_time_out <- best_time_df %>%
      arrange(SEK_per_kWh_usage_time)
  })

  output$best_time_df_table <- renderTable({
    req(df_best_time_from_now())
    df_best_time_from_now()
  })

  output$text_start_device <- renderText({

    req(df_best_time())
    req(df_best_time_from_now())

    first_best_time <- df_best_time() %>%
      slice(1) %>%
      mutate(time_end_best_time = time_start + hours(input$usage_time))


    paste0("You should start you device in ",
          "<font color=\"#FFA500\"><b>",
          as.integer(df_best_time_from_now()[1]$hours_until_start),
          "</b></font> hours, at
          <font color=\"#FFA500\"><b>",
          df_best_time_from_now()[1]$time_start_from_time_now,
          "</b></font>.</br>
          Best full hour to start: ",
          "<font color=\"#3CB371\"><b>",
          first_best_time$time_start,
          "</b></font>.") })


}
