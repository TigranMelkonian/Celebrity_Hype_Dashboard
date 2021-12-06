source("source_wikipedia_data.R")

ui <- dashboardPage(
<<<<<<< HEAD
=======
  
>>>>>>> f337570c9492308db33a56db284890a2828cee7b
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
<<<<<<< HEAD
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

=======
      menuItem("Wikipedia Page Views", tabName = "celebritywikipediapageviews", icon = icon("table", lib = "font-awesome", "fa-1x"))
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
      
>>>>>>> f337570c9492308db33a56db284890a2828cee7b
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
            width = 4, align = "left",
            withTags({
              div(
                class = "body", checked = NA,
                h3("Celebrity Hype ", style = "font-weight: bold;text-align:center", a(img(class = "img-square", src = "chart-line-solid.png"))),
                p("The Celebrity Hype tab contains a trends search feature that produces a graphical representation of the relative scale of public interest (hype..) for your favorite celebrities! The tool allows for the tracking of various celebrities and search terms that have a verified Wikipedia page."),
                p("The primary goal of this feature is to help the user understand how much hype is behind specific celebrity life events. For example: if news breaks involving Lori Loughlin and a college admissions scandal, you would be able to understand how much hype it resulted in and how long it lasted. Furthermore you could compare the resulting hype of Lori's admission scandal against Felicity Hufman's college admission scandal to draw indirect insight into how impactful an event was given relative context."),
                p("You are encouraged to play around with custom search queries, data ranges, and platform designations to view whether a celebrity is on the rise or declining across all user platforms (desktop, mobile-app,and mobile-web)."),
                p("If you aren't able to find a celebrity in the preloaded celebrity list, simply add the query to the celebrity table by typing in your desired celebrity and selecting the top search result ('Add Query...')."),
                p("Though it is possible to add any query to the celebrity table, it is recommended that you only add celebrity names and no other search terms to ensure propper functionality of the tool."),
                p("However, if you would like to mix in custom search queries that are not celebrity names, it is reconmended that you either vizualize the hype trend by it's self or compare among like search types (i.e. celebrities with celebrities, companies with companies, trend topics with trend topics, etc.). This will ensure that you are searching across simillar search volume scales and that no search term is unintentionally washed out or blown out of proportion.")
              )
            })
          ),
          column(
            width = 4, align = "left",
            withTags({
              div(
                class = "body", checked = NA,
                h3("Wikipedia Page Views ", style = "font-weight: bold;text-align:center;", a(img(class = "img-square", src = "table-solid.png"))),
                p("The Wikipedia Page Views tab contains raw downloadable Wikipedia page view data-tables for the celebrities and date-range you specified on the Celebrity Hype tab"),
                p("Wikipedia page views refers to the number of times a particular Wikipedia page has been requested. Using the R package 'pageviews', it is possible to see statistics on how often any Wikipedia page has been viewed during various times, including data as far back as July 1, 2015. These figures do not reflect the number of unique visitors a page has received."),
                p("The statistics do not consider how long people have stayed at the article. Whether they load the article and read from begin to end, or if they leave it in seconds, it will count as one view."),
                p("It is important to note that in some rare cases pageviews might have been purposely manipulated by the use of scripts or malware."),
                p("For more information about Wikipedia Page View Data, please refer to this", a(href = "https://en.wikipedia.org/wiki/Wikipedia:Pageview_statistics", "link."))
              )
            })
          ),
          column(
            width = 4, align = "left",
            withTags({
              div(
                class = "body", checked = NA,
                h3("Methodology ", style = "font-weight: bold;text-align:center;", a(img(class = "img-square", src = "flask-solid.png"))),
                p("Raw daily wikipedia pageview data is transformed to mimic the Google Trends score format! For more information on how Google generates its trend score for any given queery, please refer to this", a(href = "https://support.google.com/trends/answer/4365533?hl=en", "link.")),
                p(
                  h4("Celebrity Hype Score Standardization Process", style = "font-weight: bold;"),
                  h5("Search Window:", style = "font-weight: bold;"),
                  p("The script takes into account the users designated search window to generate relative hype scores. Generally, for comparative analyses, large window size  help flush out insignificant spikes in the genereated weekly hype scores and highlight true variations in hype for a two or more celebrities (search term) over time"),
                  h5("Score calculation:", style = "font-weight: bold;"),
                  p("In creating the incident score, the daily pageview data is first grouped by total weekly pageview volume and divided by the total pageview volume of the trailing search window designated by the user. These ratios are finally rescaled on a range of 0-100 based on a week's respoective pageview volume proportion to the total pageview volume in the designated trailing window.")
                )
              )
            })
          )
        )
      ),
<<<<<<< HEAD

      ##########################
      # Celebrity Hype Trends #
      ########################

=======
      
      ##########################
      # Celebrity Hype Trends #
      ########################
      
>>>>>>> f337570c9492308db33a56db284890a2828cee7b
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
<<<<<<< HEAD

      ####################################
      # pageview data tables / download #
      ###################################

=======
      
      ####################################
      # pageview data tables / download #
      ###################################
      
>>>>>>> f337570c9492308db33a56db284890a2828cee7b
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
<<<<<<< HEAD
=======

>>>>>>> f337570c9492308db33a56db284890a2828cee7b
            fluidRow(
              style = "padding-top:101px;text-slign:left;",
              downloadButton("download", "Download")
            )
          )
        )
<<<<<<< HEAD
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
          verbatimTextOutput('tukeyhsdresults')
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
=======
>>>>>>> f337570c9492308db33a56db284890a2828cee7b
      )
    )
  )
)
