dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Premier League"),
  dashboardSidebar(sidebarMenu(
    menuItem(
      text = "EPL Overview",
      tabName = "menu_1",
      icon = icon("globe")
    ),
    
    menuItem(
      text = "Team Statistics",
      tabName = "menu_2",
      icon = icon("chart-simple")
    ),
    
    menuItem(
      text = "Goal Analysis",
      tabName = "menu_3",
      icon = icon("futbol")
    ),
    
    menuItem(
      text = "Data",
      tabName = "menu_4",
      icon = icon("table")
    )
    
  )),
  dashboardBody(tabItems(
    tabItem(tabName = "menu_1", 
            h2("English Premier Overview of 22 Seasons (2000-2022)"),
            fluidRow(
              valueBox(length(unique(epl_clean$Team)), "TOTAL TEAMS", 
                       icon = icon("futbol"),
                       color = "green"
                       ),
              valueBox(last_champion, "LAST CHAMPION", 
                       icon = icon("trophy"),
                       color = "light-blue"
              ),
              valueBox(most_champion, "MOST CHAMPIONS", 
                       icon = icon("medal"),
                       color = "red"
              ),
              box(
                width = 12,
                checkboxGroupButtons(
                  inputId = "input_result",
                  label = "Results",
                  choices = unique(epl_winner$name),
                  selected = unique(epl_winner$name),
                  justified = TRUE,
                  checkIcon = list(
                    yes = icon("ok", 
                               lib = "glyphicon"))
                ),
              
              box(
                width = 12, height = 450,
                plotlyOutput(outputId = "plotly_1")
              )
              )
            )),
    
    tabItem(tabName = "menu_2",
            fluidRow(
              box(
                width = 12,
                selectInput(
                  inputId = "input_team",
                  label = "Please select team",
                  choices = unique(epl_clean$Team)
                )
              ),
              
              box(width = 12,
                  plotlyOutput(outputId = "plotly_2")),
              box(width = 2,height = 600,
                  radioButtons(inputId = "input_season",
                               label = "Select Season",
                               choices = unique(epl_clean$Season,),
                               selected = "2021-22"
                  )),
              box(width = 6,height = 600,
                  plotlyOutput(outputId = "plotly_3")),
              box(width = 4,height = 600,
                  plotlyOutput(outputId = "plotly_4"))
            )),
    
    tabItem(tabName = "menu_3",
            fluidRow(
              box(
                width = 12,
                selectInput(
                  inputId = "input_Season",
                  label = "Please Select Season",
                  choices = unique(epl_clean$Season),
                  selected = "2021-22"
                )
              ),
              valueBoxOutput("most_goal"),
              valueBoxOutput("most_conceded"),
              valueBoxOutput("total_goal"),
              valueBoxOutput("pos_1"),
              valueBoxOutput("pos_20"),
              valueBoxOutput("average_goal"),
              box(
                width = 4,
                title = "Number of Goal & Position Correlation",
                plotlyOutput(outputId = "plotly_6")
              ),
              box(
                width = 4,
                title = "Number of Conceded & Position Correlation",
                plotlyOutput(outputId = "plotly_7")
              ),
              box(
                width = 4,
                title = "Number of Goal & Conceded Correlation",
                plotlyOutput(outputId = "plotly_8")
              )
            )),
    
    tabItem(tabName = "menu_4",
            dataTableOutput(outputId = "df"))
    
  ))
)
