source("source_wikipedia_data.R")


server <- function(input, output, session) {

  #################################################################################################
  #                                       DATA OUTPUT ELEMENTS                                   #
  ###############################################################################################


  #############################
  # page view raw data table #
  ###########################

  output$wikipedia_df_raw <- renderDataTable({
    # initialize updated data for data table output
    wikipedia_raw_df <- session_data_input() %>%
      select(
        "fullname",
        "week_start",
        "weekly_pageviews"
      ) %>%
      arrange(desc(fullname))
    # set custome features for data table output
    datatable(wikipedia_raw_df,
      options = list(
        paging = T, scrollX = F, searching = T, bInfo = T,
        autoWidth = F,
        class = "cell-border hover compact",
        pageLength = 10,
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
          "}"
        )
      ),
      escape = F
    )
  })

  #######################################
  # page view summary stats data table #
  #####################################

  output$summarystats <- renderDataTable({
    # initialize updated data for data table output
    wikipedia_summary_df <- session_data_input() %>%
      select(
        fullname,
        week_start,
        weekly_pageviews
      ) %>%
      group_by(fullname) %>%
      summarise(
        search_week_start = min(week_start),
        search_week_end = max(week_start),
        min_weekly_pageviews = min(weekly_pageviews),
        median_weekly_pageviews = median(weekly_pageviews),
        mean_weekly_pageviews = round(mean(weekly_pageviews), 2),
        max_weekly_pageviews = max(weekly_pageviews),
        stdin_weekly_pageviews = round(sd(weekly_pageviews), 2),
        total_pageviews = sum(weekly_pageviews)
      )
    # set custome features for data table output
    datatable(wikipedia_summary_df,
      options = list(
        paging = T, scrollX = T, searching = T, bInfo = T,
        autoWidth = F,
        class = "cell-border hover compact",
        pageLength = 10,
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
          "}"
        )
      ),
      escape = F
    )
  })

  ########################################################
  # Celebrity Hype Trends bar plot - average hype score #
  ######################################################

  output$celebritytrendbarplot <- renderPlotly({
    # initialize updated data for bar plot output
    rescaled_wikipedia_df <- session_data_input()
    aggregated_rescaled_wikipedia_df <- rescaled_wikipedia_df[, c(1, 5)]
    aggregated_rescaled_wikipedia_df <- aggregate(aggregated_rescaled_wikipedia_df, by = list(aggregated_rescaled_wikipedia_df$fullname), FUN = mean)
    aggregated_rescaled_wikipedia_df <- aggregated_rescaled_wikipedia_df[, c(1, 3)]
    colnames(aggregated_rescaled_wikipedia_df) <- c("fullname", "avg_hype_score")
    aggregated_rescaled_wikipedia_df$avg_hype_score <- round(aggregated_rescaled_wikipedia_df$avg_hype_score, 0)
    ggplot(data = aggregated_rescaled_wikipedia_df, aes(x = fullname, y = avg_hype_score, colour = fullname)) +
      geom_bar(stat = "identity", fill = "white", group = aggregated_rescaled_wikipedia_df$fullname) +
      xlab("Average Hype Score") +
      ylab(" ") +
      ylim(0, 100) +
      scale_color_brewer(palette = "Dark2") +
      theme_classic() +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        legend.position = "none"
      ) -> P
    plotly::ggplotly(P, height = 480.2, tooltip = c("x", "y")) %>% layout(dragmode = "select", paper_bgcolor = "transparent")
  })

  ######################################################
  # Celebrity Hype Trends line plot  - hype over time #
  ####################################################

  output$celebritytrendplot <- renderPlotly({
    # initialize updated data for trend plot
    rescaled_wikipedia_df <- session_data_input()
    ggplot(data = rescaled_wikipedia_df, aes(x = week_start, y = hype_score, colour = fullname)) +
      geom_line(group = rescaled_wikipedia_df$fullname) +
      geom_point(aes(color = fullname), size = 2) +
      xlab(" ") +
      ylab(" ") +
      ggtitle("Trend Over Time") +
      ylim(0, 100) +
      scale_color_brewer(palette = "Dark2") +
      theme_classic() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        panel.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "transparent")
      ) -> P
    plotly::ggplotly(P, height = 500, tooltip = c("x", "y")) %>% layout(dragmode = "select", paper_bgcolor = "transparent")
  })

  ####################################
  # Download handler - pageview raw #
  ##################################

  output$download <- downloadHandler(
    filename = function() {
      paste0("wikipedia_df_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- (session_data_input() %>%
        select("fullname", "week_start", "weekly_pageviews") %>%
        arrange(desc(fullname)))
      data$fullname <- gsub("_", " ", data$fullname)

      write.csv(data,
        file,
        row.names = F
      )
    }
  )

  ##############################################
  # Download handler - pageview summary stats #
  ############################################

  output$downloadsummary <- downloadHandler(
    filename = function() {
      paste0("wikipedia_df_summary_stats_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- session_data_input() %>%
        select(
          fullname,
          week_start,
          weekly_pageviews
        ) %>%
        group_by(fullname) %>%
        summarise(
          search_start_date = min(week_start),
          search_end_date = max(week_start),
          min_weekly_pageviews = min(weekly_pageviews),
          median_weekly_pageviews = median(weekly_pageviews),
          mean_weekly_pageviews = round(mean(weekly_pageviews), 2),
          max_weekly_pageviews = max(weekly_pageviews),
          stdin_weekly_pageviews = round(sd(weekly_pageviews), 2),
          total_weekly_pageviews = sum(weekly_pageviews)
        )

      write.csv(data,
        file,
        row.names = F
      )
    }
  )

  #############################################
  # Celebrity Hype Summary Stats - Histogram #
  ###########################################

  output$celebrityhypehist <- renderPlotly({
    # initialize updated data for trend plot
    rescaled_wikipedia_df <- session_data_input()
    ggplot(data = rescaled_wikipedia_df, aes(x = fullname, y = hype_score, colour = fullname)) +
      geom_boxplot(notch = TRUE, width = 0.5 / length(unique(rescaled_wikipedia_df$fullname))) +
      xlab("Celebrity") +
      ylab("Hype Score") +
      ylim(0, 100) +
      scale_color_brewer(palette = "Dark2") +
      theme_classic() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        panel.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "transparent")
      ) -> P
    plotly::ggplotly(P, height = 500, tooltip = c("x", "y")) %>% layout(dragmode = "select", paper_bgcolor = "transparent")
  })

  output$celebrityhypesourcebar <- renderPlotly({
    # initialize updated data for trend plot
    rescaled_wikipedia_df <- session_data_input()
    rescaled_wikipedia_df <- rescaled_wikipedia_df %>%
      group_by(fullname) %>%
      summarise(
        weekly_pageviews = sum(weekly_pageviews)
      )

    ggplot(data = rescaled_wikipedia_df, aes(x = fullname, y = weekly_pageviews, fill = fullname)) +
      geom_bar(stat = "identity") +
      xlab("Celebrity") +
      ylab("View Count") +
      scale_color_brewer(palette = "Dark2") +
      theme_classic() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        panel.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "transparent")
      ) -> P
    plotly::ggplotly(P, height = 500, tooltip = c("x", "y")) %>% layout(dragmode = "select", paper_bgcolor = "transparent")
  })

  output$celebrityhypedensity <- renderPlotly({
    # initialize updated data for trend plot
    rescaled_wikipedia_df <- session_data_input()
    rescaled_wikipedia_df <- rescaled_wikipedia_df %>%
      group_by(fullname, month_start = cut(week_start, "month")) %>%
      summarise(
        monthly_pageviews = sum(weekly_pageviews)
      )
    ggplot(data = rescaled_wikipedia_df, aes(x = month_start, y = monthly_pageviews, group = fullname, color = fullname, fill = fullname)) +
      geom_point(size = 2) +
      stat_smooth(method = lm, formula = y ~ poly(x, 4), se = TRUE) +
      xlab("Date") +
      ylab("View Count") +
      scale_color_brewer(palette = "Dark2") +
      theme_classic() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        panel.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "transparent")
      ) -> P
    plotly::ggplotly(P, height = 500, tooltip = c("x", "y")) %>% layout(dragmode = "select", paper_bgcolor = "transparent")
  })
  
  output$tukeyhsdresults <- renderPrint({
    TukeyHSD(anovaresults())
  })

  output$anovafitplots <- renderPlot({
    par(mfrow=c(2,2))
    plot(anovaresults())
    par(mfrow=c(1,1))
  })
  
  #################################################################################################
  #                                       REACTIVE DATA UPDATE ELEMENTS                          #
  ###############################################################################################

  # Reactive expressions are smarter than regular R functions.
  # They cache their values and know when their values have become outdated.
  session_data_input <- reactive({
    # initialize global server wikipedia data frame
    wikipedia_df <- source_wikipedia_data(celebrities = req(input$dropdownselectceleb), date_range = input$daterangetrenddate, input$platforminput)
    wikipedia_df$date <- as.Date(wikipedia_df$date, format = "%Y-%m-%d")
    wikipedia_df$fullname <- gsub("_", " ", wikipedia_df$fullname)
    groupped_wikipedia_df <- group_wikipedia_df(wikipedia_df)
    rescaled_wikipedia_df <- generate_trend_scores(groupped_wikipedia_df)
    return(rescaled_wikipedia_df)
  })
  
  # Reactive expressions are smarter than regular R functions.
  # They cache their values and know when their values have become outdated.
  anovaresults <- reactive({
    rescaled_wikipedia_df <- session_data_input()
    rescaled_wikipedia_df$fullname <- as.factor(rescaled_wikipedia_df$fullname) 
    result <- aov(weekly_pageviews ~ fullname, data = rescaled_wikipedia_df)
    return(result)
  })
}
