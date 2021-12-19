source("source_wikipedia_data.R")

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = "Hype Dashboard",
    tags$li(
      class = "dropdown",
      tags$style(".main-header {max-height: 50px;}"),
      tags$style(".main-header .logo {height: 55px;padding-top: 5px !important;}"),
      tags$style(".sidebar-toggle {height: 55px; padding-top: 20px !important;}"),
      tags$style(".navbar {min-height:45px !important;}")
    ),
    tags$li(class = "dropdown", tags$a(class = "btn", href = "https://tigranmelkonian.github.io/webPage/", tags$img(src = "globe.png", title = "Personal web page!", height = 21, width = 21))),
    tags$li(class = "dropdown", tags$a(class = "btn", href = "https://github.com/TigranMelkonian", tags$img(src = "github.png", title = "GitHub", height = 17, width = 17)))
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar", # id important for updateTabItems
      menuItem("Home", tabName = "home", icon = icon("home", lib = "font-awesome", "fa-1x")),
      menuItem("Celebrity Hype", tabName = "celebrityhypetrends", icon = icon("chart-line", lib = "font-awesome", "fa-1x")),
      menuItem("Wikipedia Page Views", tabName = "celebritywikipediapageviews", icon = icon("table", lib = "font-awesome", "fa-1x")),
      menuItem("Celebrity Statistics", tabName = "celebrityhypesummarystats", icon = icon("flask", lib = "font-awesome", "fa-1x"))
    )
  ),
  dashboardBody(

    #################################################################################################
    #                                       Main Dashbaord Body                                    #
    ###############################################################################################

    tabItems(

      ###############################
      # Home - dashboard info text #
      #############################

      tabItem(
        "home",
        withTags({
          div(
            class = "header", checked = NA, style = "text-align:center;",
            h1("Welcome to the Celebrity Hype Shiny Dashboard!"),
            a(img(class = "img-circle", src = "trend_logo.png", alt = "User Avatar"))
          )
        }),
        fluidRow(
          column(
            width = 12, align = "left",
            withTags({
              div(
                class = "body", checked = NA, style = "width:100%;height:100%;",
                tags$iframe(allowtransparency = "true", style = "height:800px; width:100%;background: white;", src = "main.pdf#toolbar=0&navpanes=0&scrollbar=0", frameborder = "0")
              )
            })
          )
        )
      ),

      ##########################
      # Celebrity Hype Trends #
      ########################

      tabItem(
        "celebrityhypetrends",
        titlePanel(div("Celebrity Hype Trends", style = "font-size: 32px; padding-bottom: 20px")),
        fluidRow(
          column(
            4,
            selectizeInput(
              "dropdownselectceleb",
              "Select Celebrity",
              choices = sort(unique(as.character(celebrity_table$fullname))),
              selected = c("Kim Kardashian West", "Kanye West"),
              options = list(create = TRUE),
              multiple = T
            )
          ),
          column(
            4,
            dateRangeInput(
              "daterangetrenddate",
              "Select Date Range",
              start = as.Date(format(Sys.Date(), "%Y-%m-%d")) - 730,
              end = format((Sys.Date() - 1), "%Y-%m-%d"),
              min = format(as.Date("2016-07-01"), "%Y-%m-%d"),
              max = as.Date(format(Sys.Date(), "%Y-%m-%d")) - 1
            ),
            tags$style(HTML(".datepicker {z-index:99999 !important;}"))
          ),
          column(
            4,
            selectInput(
              "platforminput",
              "Select platform",
              choices = c("all", "desktop", "mobile-app", "mobile-web"),
              selected = c("all"),
              multiple = F
            )
          ),
          fluidRow(column(8, plotlyOutput("celebritytrendplot", height = "100%")), column(4, plotlyOutput("celebritytrendbarplot", height = "100%")))
        )
      ),

      ####################################
      # pageview data tables / download #
      ###################################

      tabItem(
        "celebritywikipediapageviews",
        fluidRow(
          column(
            10,
            withTags({
              div(
                class = "header", checked = NA, style = "text-align:center;",
                h2("Page View Summary Statistics")
              )
            }),
            div(dataTableOutput("summarystats"),
              style = "font-size: 85%; width: 100%;"
            )
          ),
          column(
            2,
            fluidRow(
              style = "padding-top:61px;text-align:left;",
              downloadButton("downloadsummary", "Download")
            )
          )
        ),
        fluidRow(
          column(
            10,
            withTags({
              div(
                class = "header", checked = NA, style = "text-align:center; padding-top: 40px",
                h2("Raw Wikipedi Page Views")
              )
            }),
            div(dataTableOutput("wikipedia_df_raw"),
              style = "font-size: 85%; width: 100%;"
            )
          ),
          column(
            2,
            fluidRow(
              style = "padding-top:101px;text-slign:left;",
              downloadButton("download", "Download")
            )
          )
        )
      ),
      tabItem(
        "celebrityhypesummarystats",
        titlePanel(div("Celebrity Statistics", style = "font-size: 32px; padding-bottom: 20px")),
        fluidRow(
          withTags({
            div(
              class = "header", checked = NA, style = "text-align:center; padding-top: 40px",
              h2("Box Plot: Hype Score Distribution")
            )
          }),
          plotlyOutput("celebrityhypehist", height = "100%")
        ),
        fluidRow(
          withTags({
            div(
              class = "header", checked = NA, style = "text-align:center; padding-top: 40px",
              h2("Bar Plot: Total Page Views v.s Celebrity")
            )
          }),
          plotlyOutput("celebrityhypesourcebar", height = "100%")
        ),
        fluidRow(
          withTags({
            div(
              class = "header", checked = NA, style = "text-align:center; padding-top: 40px",
              h2("Line Plot: Total Page Views v.s. Date")
            )
          }),
          plotlyOutput("celebrityhypedensity", height = "100%")
        ),
        fluidRow(
          withTags({
            div(
              class = "header", checked = NA, style = "text-align:center; padding-top: 40px",
              h2("Tukey Honest Significant Differences (TukeyHSD)")
            )
          }),
          verbatimTextOutput("tukeyhsdresults")
        ),
        fluidRow(
          withTags({
            div(
              class = "header", checked = NA, style = "text-align:center; padding-top: 40px",
              h2("Anova Fit Plots")
            )
          }),
          plotOutput("anovafitplots")
        )
      )
    )
  )
)
