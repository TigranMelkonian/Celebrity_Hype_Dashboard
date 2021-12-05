library(rsconnect)
library(shiny)
library(shinydashboard)
library(lubridate)
library(dplyr)
library(DT)
library(shinyWidgets)
library(shinyjs)
library(plotly)
library(ggplot2)
library(data.table)
library(tidyr)
library(RColorBrewer)
library(pageviews)
library(scales)
library(ggridges)

# load in celebrity data table
celebrity_table <- read.csv(paste0(getwd(), "/celebrity_wikipedia_url_dt.csv"))

###############################
# SOURCING HELPER FUNCTIONS ##
#############################


# Expects: raw daily page view data sourced from 'source_wikipedia_data' function as input
# Does: Aggregates daily page view volume to weekly page view volumes
# Returns: Data table including celebrity name, date of week start, and weekly page view volume
group_wikipedia_df <- function(raw_wikipedia_view_data) {
  grouped_wikipedia_pageviews <- raw_wikipedia_view_data %>%
    select(fullname, date, views) %>%
    group_by(fullname, week_start = cut(date, "week")) %>%
    summarise(weekly_pageviews = sum(views, na.rm = T)) %>%
    mutate(week_start = as.Date(week_start))
  return(grouped_wikipedia_pageviews)
}


# Expects: A grouped page view data table 
# Does: Rescales weekly page view volume values from 0 - 100 using relative
#       global min page view and max page view
# Returns: Data table including celebrity name, date of week start, and weekly hype score 
generate_trend_scores <- function(grouped_wikipedia_data) {
  search_window_volume <- sum(grouped_wikipedia_data$weekly_pageviews)
  grouped_wikipedia_data$pageview_ratio <- as.double((grouped_wikipedia_data$weekly_pageviews) / search_window_volume)
  grouped_wikipedia_data$hype_score <- ""
  min <- as.double(min(grouped_wikipedia_data$pageview_ratio, na.rm = T))
  max <- as.double(max(grouped_wikipedia_data$pageview_ratio, na.rm = T))
  for (i in 1:nrow(grouped_wikipedia_data)) {
    grouped_wikipedia_data$hype_score[i] <- rescale(grouped_wikipedia_data$pageview_ratio[i], from = c(min, max), to = c(0, 100))
  }
  grouped_wikipedia_data$hype_score <- as.integer(grouped_wikipedia_data$hype_score)
  return(grouped_wikipedia_data)
}


# Expects: List of celebrities, list containing two dates objects from - to, 
#          desired platform i.e.(mobile, desktop, mobile-web, or all)
# Does: Uses 'pageviews' package to retrieve wiki article page views for designated 
#       celebrities within a specified date range observed on a specified user platform
# Returns: Data table including celebrity name, date, and daily wikipedia page view volume
source_wikipedia_data <- function(celebrities, date_range, platform) {
  for (i in 1:length(celebrities)) {
    if (i == 1) {
      if (celebrities[i] %in% celebrity_table$fullname) {
        wikipedia_df <- tryCatch({
          data.frame(article_pageviews(article = gsub("https://en.wikipedia.org/wiki/", "", as.character(celebrity_table[celebrity_table$fullname == celebrities[i], "wikipediaurl"])), start = pageview_timestamps(date_range[1]), end = pageview_timestamps(date_range[2]), platform = platform))
        }, error = function(e) {
          data.frame(article = celebrities[i])
        })
      } else {
        wikipedia_df <- tryCatch({
          data.frame(article_pageviews(article = gsub(" ", "_", celebrities[i]), start = pageview_timestamps(date_range[1]), end = pageview_timestamps(date_range[2]), platform = platform))
        }, error = function(e) {
          data.frame(article = celebrities[i])
        })
      }
    } else {
      if (celebrities[i] %in% celebrity_table$fullname) {
        wikipedia_df <- plyr::rbind.fill(wikipedia_df, tryCatch({
          data.frame(article_pageviews(article = gsub("https://en.wikipedia.org/wiki/", "", as.character(celebrity_table[celebrity_table$fullname == celebrities[i], "wikipediaurl"])), start = pageview_timestamps(date_range[1]), end = pageview_timestamps(date_range[2]), platform = platform))
        }, error = function(e) {
          data.frame(article = celebrities[i])
        }))
      } else {
        wikipedia_df <- plyr::rbind.fill(wikipedia_df, tryCatch({
          data.frame(article_pageviews(article = gsub(" ", "_", celebrities[i]), start = pageview_timestamps(date_range[1]), end = pageview_timestamps(date_range[2]), platform = platform))
        }, error = function(e) {
          data.frame(article = celebrities[i])
        }))
      }
    }
  }

  wikipedia_df <- wikipedia_df[, c(3, 7, 8)]
  colnames(wikipedia_df) <- c("fullname", "date", "views")
  return(wikipedia_df)
}


# Expects: NA
# Does: To run after any final updates were made to this Shiny App 
#       so that updates are reflected on version hosted on shiny.io server
# Returns: NA
update_shinyio <- function() {
  rsconnect::deployApp(paste0(getwd()))
}
