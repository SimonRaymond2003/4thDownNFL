# ui.R

# Define UI
ui <- navbarPage(
  "NFL Analysis (2016-2023)",
  useShinyjs(),
  tags$head(
    tags$script("
      $(document).ready(function(){
        // Show info modal on tab load
        $('a[data-value=\"Player Statistics\"]').on('shown.bs.tab', function(){
          $('#info_modal').show();
        });
        
        // Close modal when clicking X
        $(document).on('click', '#close_modal', function(){
          $('#info_modal').hide();
        });
        
        // Close modal when clicking outside
        $(window).click(function(event) {
          if ($(event.target).is('#info_modal')) {
            $('#info_modal').hide();
          }
        });
      });
    ")
  ),
  
  # Tab 1: Formation Viewer
  tabPanel("Formation Viewer",
           sidebarLayout(
             sidebarPanel(
               selectInput("year", "Season:", choices = NULL),
               selectInput("week", "Week:", choices = NULL),
               selectInput("game", "Game:", choices = NULL),
               selectInput("play", "Play:", choices = NULL),
               verbatimTextOutput("play_details"),
               width = 3
             ),
             mainPanel(
               plotOutput("formation_plot", height = "500px"),
               htmlOutput("play_desc"),
               fluidRow(
                 column(6,
                        h4("Offensive Players", style = "background-color: #f8f9fa; padding: 10px;"),
                        DT::dataTableOutput("offense_table")
                 ),
                 column(6,
                        h4("Defensive Players", style = "background-color: #f8f9fa; padding: 10px;"),
                        DT::dataTableOutput("defense_table")
                 )
               )
             )
           )
  ),
  # tab 2
  
  # Tab 2: Player Statistics
  tabPanel("Player Statistics",
           # Data display notice
           div(
             style = "background-color: #f8f9fa; border-left: 5px solid #007bff; padding: 15px; margin: 15px 0; border-radius: 4px;",
             h4("Important Notice About Data Display", style = "color: #007bff; margin-top: 0;"),
             p("While I have personally created and engineered this dataset from PFF player reports for my research and analysis, I am currently awaiting confirmation regarding public display permissions. Until this is confirmed, numerical values will be displayed as '###'"),
             p("The complete dataset with all numerical values is available for my personal analytical use. This masking is temporary while permissions are finalized.")
           ),
           
           # Information Modal
           div(
             id = "info_modal",
             style = "display: none; position: fixed; z-index: 1000; left: 0; top: 0; width: 100%; height: 100%; overflow: auto; background-color: rgba(0,0,0,0.4);",
             div(
               style = "background-color: #fefefe; margin: 15% auto; padding: 20px; border: 1px solid #888; width: 80%; max-width: 700px; position: relative;",
               tags$span(
                 id = "close_modal",
                 HTML("&times;"),
                 style = "position: absolute; right: 10px; top: 5px; font-size: 28px; font-weight: bold; cursor: pointer;"
               ),
               h3("About Player Statistics", style = "margin-top: 10px;"),
               p("This tab allows you to explore player statistics. This data has been engineered from PFF player reports.
               You can view both individual week performance and aggregated statistics over selected time periods."),
               h4("Features:"),
               tags$ul(
                 tags$li("Select specific weeks and years to analyze player performance"),
                 tags$li("View detailed weekly statistics with horizontal scrolling"),
                 tags$li("See aggregated statistics over custom time periods"),
                 tags$li("Calculate averages excluding NA values"),
                 tags$li("Filter by position, team, and player")
               )
             )
           ),
           
           # Main Content
           fluidRow(
             # Left Sidebar
             column(3,
                    wellPanel(
                      actionButton("show_info", "Show Information", class = "btn-info"),
                      br(), br(),
                      selectInput("player_position", "Select Position:", 
                                  choices = c("Select Position" = "",
                                              "K", "QB", "WR", "ED", "CB", "TE", "DI", "S", 
                                              "LB", "P", "LS", "T", "HB", "G", "C", "FB"),
                                  selected = ""),
                      selectInput("player_team", "Select Team:", 
                                  choices = c("Select Team" = ""),
                                  selected = ""),
                      selectInput("player_name", "Select Player:", 
                                  choices = c("Select Player" = ""),
                                  selected = ""),
                      sliderInput("year_range", "Select Years:",
                                  min = 2016, max = 2023,
                                  value = c(2016, 2023),
                                  step = 1,
                                  sep = ""),
                      sliderInput("week_range", "Select Weeks:",
                                  min = 1, max = 18,
                                  value = c(1, 18),
                                  step = 1)
                    )
             ),
             
             # Main Panel
             column(9,
                    # Position Summary
                    div(
                      style = "background-color: #f8f9fa; padding: 15px; margin-bottom: 20px; border-radius: 5px;",
                      h4("Position Summary", style = "margin-top: 0;"),
                      verbatimTextOutput("position_summary")
                    ),
                    
                    # Statistics Tabs
                    tabsetPanel(id = "stats_tabs",
                                # Weekly Stats Tab
                                tabPanel("Weekly Stats",
                                         div(
                                           style = "overflow-x: auto;",
                                           DT::dataTableOutput("weekly_stats")
                                         )
                                ),
                                # Aggregated Stats Tab
                                tabPanel("Aggregated Stats",
                                         div(
                                           style = "overflow-x: auto; width: 100%;",
                                           DT::dataTableOutput("aggregated_stats", width = "100%")
                                         )
                                )
                    )
             )
           )
  ),
  # Tab 3: Fourth Down Analysis
  tabPanel("Fourth Down Analysis",
           sidebarLayout(
             sidebarPanel(
               selectInput("analysis_year", "Select Year:",
                           choices = 2016:2023,
                           selected = 2023),
               
               sliderInput("analysis_weeks", "Select Weeks:",
                           min = 1, max = 18,
                           value = c(1, 18),
                           step = 1),
               
               checkboxInput("select_all_teams", "Select/Deselect All Teams", TRUE),
               
               div(
                 style = "max-height: 300px; overflow-y: auto;",
                 checkboxGroupInput("selected_teams", "Select Teams:",
                                    choices = c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", 
                                                "CLE", "DAL", "DEN", "DET", "GB", "HOU", "IND", 
                                                "JAX", "KC", "LAC", "LA", "LV", "MIA", "MIN", 
                                                "NE", "NO", "NYG", "NYJ", "PHI", "PIT", "SEA", 
                                                "SF", "TB", "TEN", "WAS"),
                                    selected = c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", 
                                                 "CLE", "DAL", "DEN", "DET", "GB", "HOU", "IND", 
                                                 "JAX", "KC", "LAC", "LA", "LV", "MIA", "MIN", 
                                                 "NE", "NO", "NYG", "NYJ", "PHI", "PIT", "SEA", 
                                                 "SF", "TB", "TEN", "WAS"))
               ),
               width = 3
             ),
             mainPanel(
               plotOutput("distance_plot", height = "400px"),
               br(),
               plotOutput("success_plot", height = "400px"),
               wellPanel(
                 verbatimTextOutput("analysis_summary")
               )
             )
           )
  ),
  
  # Tab 4: Fourth Down Decisions
  tabPanel("Fourth Down Decisions",
           sidebarLayout(
             sidebarPanel(
               selectInput("decisions_year", "Select Year:",
                           choices = 2016:2023,
                           selected = 2023),
               
               selectInput("conference", "Conference:",
                           choices = c("All", "AFC", "NFC"),
                           selected = "All"),
               
               selectInput("division", "Division:",
                           choices = c("All", "North", "South", "East", "West"),
                           selected = "All"),
               
               checkboxInput("select_all_teams_decisions", 
                             "Select/Deselect All Teams", 
                             TRUE),
               
               div(
                 style = "max-height: 300px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; border-radius: 5px;",
                 checkboxGroupInput("selected_teams_decisions", "Select Teams:",
                                    choices = c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", 
                                                "CLE", "DAL", "DEN", "DET", "GB", "HOU", "IND", 
                                                "JAX", "KC", "LAC", "LA", "LV", "MIA", "MIN", 
                                                "NE", "NO", "NYG", "NYJ", "PHI", "PIT", "SEA", 
                                                "SF", "TB", "TEN", "WAS"),
                                    selected = c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", 
                                                 "CLE", "DAL", "DEN", "DET", "GB", "HOU", "IND", 
                                                 "JAX", "KC", "LAC", "LA", "LV", "MIA", "MIN", 
                                                 "NE", "NO", "NYG", "NYJ", "PHI", "PIT", "SEA", 
                                                 "SF", "TB", "TEN", "WAS"))
               ),
               
               div(
                 style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-top: 20px;",
                 h4("Summary Statistics", style = "margin-top: 0;"),
                 verbatimTextOutput("decisions_summary")
               ),
               
               width = 3
             ),
             mainPanel(
               fluidRow(
                 column(12,
                        div(
                          style = "background-color: white; padding: 15px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                          div(
                            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
                            h4("Decision Distribution", style = "margin: 0;"),
                            materialSwitch("toggle_top_plot", "Switch View", 
                                           status = "primary", right = TRUE)
                          ),
                          plotOutput("decisions_plot_top", height = "400px")
                        )
                 )
               ),
               br(),
               fluidRow(
                 column(12,
                        div(
                          style = "background-color: white; padding: 15px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                          div(
                            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
                            h4("Attempt Rates", style = "margin: 0;"),
                            materialSwitch("toggle_bottom_plot", "Switch View", 
                                           status = "primary", right = TRUE)
                          ),
                          plotOutput("decisions_plot_bottom", height = "400px")
                        )
                 )
               ),
               br(),
               fluidRow(
                 column(12,
                        div(
                          style = "background-color: white; padding: 15px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                          h4("Team Statistics"),
                          DT::dataTableOutput("decisions_table")
                        )
                 )
               )
             )
             # Close the Fourth Down Decisions tabPanel
           )
  ),
  
  # About tab 5
  # About tab 5
  tabPanel("About",
           fluidPage(
             fluidRow(
               column(8, offset = 2,
                      div(
                        style = "background-color: white; padding: 20px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); margin-top: 20px;",
                        h2("About the Developer", style = "border-bottom: 2px solid #f0f0f0; padding-bottom: 10px;"),
                        p("My name is Simon Raymond. I'm a fourth-year honours economics student at Saint Mary's University with a focus on econometrics, sports analytics and data science."),
                        
                        h3("Academic Experience", style = "margin-top: 20px;"),
                        p("Currently working on my Honours Project: Predictive and Casual Analysis of 4th Down Attempts in the NFL"),
                        tags$ul(
                          tags$li("GPA: 4.20/4.30"),
                          tags$li("Research Focus: Combining traditional econometric methods with modern machine learning approaches to analyze 4th down decisions. This includes implementing both Heckman Selection models and predictive tools like XGBoost and Random Forest to provide a comprehensive analysis of fourth down attempts in the NFL"),                          tags$li("Implementing Heckman Correction for casual analysis on factors affecting 4th down results"),
                          tags$li("Teaching Assistant for Machine Learning, Econometrics, and Human Resource Economics courses"),
                          tags$li("Lab instructor for Machine Learning and Artificial Intelligence, teaching students advanced algorithms and concepts using both R and Python"),
                          tags$li("Lab instructor for Econometrics, combining theoretical linear algebra foundations with practical computational applications in either R or Python"),                        ),
                        
                        h3("Research Assistant Projects", style = "margin-top: 20px;"),
                        div(
                          style = "display: flex; gap: 20px; margin-top: 15px;",
                          div(
                            style = "flex: 1; padding: 15px; border: 1px solid #ddd; border-radius: 5px;",
                            h4("Nova Scotia Neighborhood Housing App"),
                            p("Interactive dashboard for analyzing Nova Scotia housing data"),
                            tags$a(href = "https://dolphin-app-ixr9p.ondigitalocean.app/", "Visit App", target = "_blank", 
                                   style = "color: #2196F3; text-decoration: none;")
                          ),
                          div(
                            style = "flex: 1; padding: 15px; border: 1px solid #ddd; border-radius: 5px;",
                            h4("Nova Scotia Crime Dashboard"),
                            p("Interactive platform for analyzing crime statistics in Nova Scotia"),
                            tags$a(href = "https://plankton-app-z95sf.ondigitalocean.app/", "Visit App", target = "_blank", 
                                   style = "color: #2196F3; text-decoration: none;")
                          )
                        ),
                        
                        h3("Contact Information", style = "margin-top: 20px;"),
                        tags$ul(
                          tags$li(tags$b("Email: "), "simon.raymond@smu.ca"),
                          tags$li(tags$b("Phone: "), "(902) 999-1499"),
                          tags$li(tags$b("GitHub: "), tags$a(href = "https://github.com/SimonRaymond2003/4thDownNFL", "github.com/SimonRaymond2003/4thDownNFL", 
                                                             target = "_blank", style = "color: #2196F3; text-decoration: none;"))
                        )
                      )
               )
             )
           )
  )
) # Close navbarPage
