# Install/load libraries
library(rvest)
library(dplyr)
library(shiny)
library(tidyverse)
library(DT)
library(bslib)
library(plotly)


# Scrape the ESPN NCAA men's basketball stats page
page <- read_html("https://www.espn.com/mens-college-basketball/stats/player")

# Extract all tables
tables <- page %>% html_table(fill = TRUE)

# Combine the two scraped tables into one data set
players <- bind_cols(tables[[1]], tables[[2]])

# Clean up the Name column (the names have school abbreviations stuck to them)
players <- players %>%
  mutate(
    # Remove the team abbreviation (all capital letters at the end)
    Name = gsub("([A-Z]{2,})$", "", Name),
    
    # Trim extra spaces left after removing the team abbreviation
    Name = trimws(Name)
  )

# Remove the rank and games played column
players <- players %>% select(-RK, -GP)

# Make sure NA values are visible instead of being dropped
players <- players %>% mutate(across(everything(), ~ ifelse(. == "", NA, .)))

# Reorder the columns so the key stats appear first
players <- players %>%
  select(
    Name, POS, PTS, REB, AST, STL, BLK, TO,   # main stats on the left
    everything()                              # remaining stats
  )


# Define UI
ui <- fluidPage(
  
  # Add a Bootstrap theme using bslib
  theme = bs_theme(bootswatch = "minty"),
  
  # App Title
  titlePanel("NBA Draft Prospect Comparison App"),
  
  # Create navigation tabs
  tabsetPanel(
    
    # Tab 1: Player Data
    tabPanel("Player Data",
             fluidRow(
               # Sidebar
               column(4,
                      hr(),
                      h4("Player Filters"),
                      
                      div(
                        checkboxGroupInput(
                          inputId = "selected_player_pos", 
                          label = "Select Player Position:", 
                          choices = c("Guard" = "G", "Forward" = "F", "Center" = "C"),
                          selected = c("G", "F", "C")
                        ),
                        helpText("Choose which positions to include in your analysis. Checking all shows all players."),
                        style = "margin-bottom: 30px;"
                      ),
                      
                      div(
                        sliderInput(
                          inputId = "min_ppg",
                          label = "Minimum Points Per Game:",
                          min = 0,
                          max = 30,
                          value = 0,
                          step = 1
                        ),
                        helpText("Use this slider to filter out players with fewer points per game."),
                        style = "margin-bottom: 30px;"
                      ),
                      
                      div(
                        checkboxGroupInput(
                          inputId = "selected_stats",
                          label = "Select Stats to Compare:",
                          choices = c("Points" = "PTS", "Rebounds" = "REB", "Assists" = "AST", 
                                      "Steals" = "STL", "Blocks" = "BLK", "Turnovers" = "TO"),
                          selected = c("PTS", "REB", "AST", "STL", "BLK", "TO")
                        ),
                        helpText("Pick which stats to display in the comparison bar plot."),
                        style = "margin-bottom: 30px;"
                      ),
                      
                      div(
                        checkboxInput(
                          inputId = "show_barplot",
                          label = "Display the bar plot below",
                          value = TRUE
                        ),
                        helpText("Uncheck this box if you don't want to see the bar plot."),
                        style = "margin-bottom: 30px;"
                      ),
                      
                      hr(),
                      h4("Select Players for Comparison"),
                      
                      div(
                        selectInput("player1", "Select Player 1:", choices = players$Name),
                        helpText("Choose the first player to compare."),
                        style = "margin-bottom: 30px;"
                      ),
                      
                      div(
                        selectInput("player2", "Select Player 2:", choices = players$Name),
                        helpText("Choose the second player to compare."),
                        style = "margin-bottom: 30px;"
                      ),
                      
                      div(
                        selectInput("player3", "Select Player 3:", choices = players$Name),
                        helpText("Choose the third player to compare."),
                        style = "margin-bottom: 30px;"
                      )
               ),
               
               # Main content
               column(8,
                      hr(),
                      h4("Player Stats Table"),
                      dataTableOutput("players_table"),
                      br(),
                      hr(),
                      h4("Player Stat Comparison Plot"),
                      plotlyOutput("barplot", height = "600px"),
                      br(),
                      hr(),
                      h4("Summary of Top Performers"),
                      verbatimTextOutput("summary"),
                      h4("Notes"),
                      p("Stats shown are per game averages from ESPN's NCAA player data.")
               )
             )
    ),
    
    # Tab 2: Custom Player
    tabPanel("Custom Player",
             fluidRow(
               column(4,
                      hr(),
                      h4("Add Your Own Player Stats"),
                      
                      div(
                        textInput("custom_name", "Player Name:"),
                        helpText("Enter the name of your custom player."),
                        style = "margin-bottom: 30px;"
                      ),
                      
                      div(
                        selectInput("custom_pos", "Position:", choices = c("G","F","C")),
                        helpText("Select the player's position."),
                        style = "margin-bottom: 30px;"
                      ),
                      
                      div(
                        numericInput("custom_pts", "Points Per Game:", value = 0, min = 0),
                        helpText("Enter the points per game for this player."),
                        style = "margin-bottom: 30px;"
                      ),
                      
                      div(
                        numericInput("custom_reb", "Rebounds Per Game:", value = 0, min = 0),
                        helpText("Enter the rebounds per game for this player."),
                        style = "margin-bottom: 30px;"
                      ),
                      
                      div(
                        numericInput("custom_ast", "Assists Per Game:", value = 0, min = 0),
                        helpText("Enter the assists per game for this player."),
                        style = "margin-bottom: 30px;"
                      ),
                      
                      div(
                        numericInput("custom_stl", "Steals Per Game:", value = 0, min = 0),
                        helpText("Enter the steals per game for this player."),
                        style = "margin-bottom: 30px;"
                      ),
                      
                      div(
                        numericInput("custom_blk", "Blocks Per Game:", value = 0, min = 0),
                        helpText("Enter the blocks per game for this player."),
                        style = "margin-bottom: 30px;"
                      ),
                      
                      div(
                        numericInput("custom_to", "Turnovers Per Game:", value = 0, min = 0),
                        helpText("Enter the turnovers per game for this player."),
                        style = "margin-bottom: 30px;"
                      ),
                      
                      div(
                        actionButton("add_player", "Add Player"),
                        helpText("Click to add your custom player to the main Player Data tab."),
                        style = "margin-bottom: 30px;"
                      )
               )
             )
    ),
    
    # Tab 3: About
    tabPanel("About",
             hr(),
             h4("Instructions"),
             p("Use the filters to explore NCAA player stats and compare players using the bar plot."),
             p("You can also add your own player to test custom stats."),
             br(),
             h4("Data Source"),
             p("Data is scraped from ESPN’s NCAA Men’s Basketball stats page in real time.")
    )
  )
)


# Define server
server <- function(input, output, session) {
  
  # Store custom players
  custom_players <- reactiveVal(data.frame(
    Name = character(),
    POS = character(),
    PTS = numeric(),
    REB = numeric(),
    AST = numeric(),
    STL = numeric(),
    BLK = numeric(),
    TO = numeric(),
    stringsAsFactors = FALSE
  ))
  
  # Add custom player
  observeEvent(input$add_player, {
    new_player <- data.frame(
      Name = input$custom_name,
      POS = input$custom_pos,
      PTS = input$custom_pts,
      REB = input$custom_reb,
      AST = input$custom_ast,
      STL = input$custom_stl,
      BLK = input$custom_blk,
      TO = input$custom_to,
      stringsAsFactors = FALSE
    )
    custom_players(bind_rows(custom_players(), new_player))
  })
  
  # Combine original and custom players
  all_players <- reactive({
    bind_rows(players, custom_players())
  })
  
  # Update player selectors automatically when new players are added
  observe({
    updateSelectInput(session, "player1", choices = all_players()$Name)
    updateSelectInput(session, "player2", choices = all_players()$Name)
    updateSelectInput(session, "player3", choices = all_players()$Name)
  })
  
  # Filtered players (keep NA positions visible even if unchecked)
  filtered_players <- reactive({
    all_players() %>%
      filter((is.na(POS) | POS %in% input$selected_player_pos) & (PTS >= input$min_ppg))
  })
  
  # Update player dropdowns whenever the filtered list changes
  observe({
    updateSelectInput(session, "player1", choices = filtered_players()$Name)
    updateSelectInput(session, "player2", choices = filtered_players()$Name)
    updateSelectInput(session, "player3", choices = filtered_players()$Name)
  })
  
  # Selected players for comparison
  selected_players <- reactive({
    filtered_players() %>%
      filter(Name %in% c(input$player1, input$player2, input$player3))
  })
  
  output$summary <- renderText({
    df <- filtered_players()
    if(nrow(df) == 0) return("No players selected.")
    
    # Find top performer for each stat
    top_pts <- df$Name[which.max(df$PTS)]
    top_reb <- df$Name[which.max(df$REB)]
    top_ast <- df$Name[which.max(df$AST)]
    top_stl <- df$Name[which.max(df$STL)]
    top_blk <- df$Name[which.max(df$BLK)]
    top_to  <- df$Name[which.min(df$TO)]  # lower turnovers is better
    
    paste0(
      "Top scorer: ", top_pts, "\n",
      "Top rebounder: ", top_reb, "\n",
      "Top assister: ", top_ast, "\n",
      "Top stealer: ", top_stl, "\n",
      "Top shot blocker: ", top_blk, "\n",
      "Lowest turnovers: ", top_to
    )
  })
  
  # Render interactive bar plot
  output$barplot <- renderPlotly({
    if (input$show_barplot && length(input$selected_stats) > 0) {
      df <- selected_players() %>%
        select(Name, all_of(input$selected_stats)) %>%
        pivot_longer(-Name, names_to = "Stat", values_to = "Value")
      
      p <- ggplot(df, aes(x = Stat, y = Value, fill = Name)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(y = "Value", x = "Stat") +
        theme_minimal(base_size = 14)
      
      ggplotly(p)  # Makes the plot interactive
    }
  })
  
  # Render data table
  output$players_table <- renderDataTable({
    DT::datatable(
      filtered_players() %>% arrange(desc(PTS)),
      options = list(pageLength = 10),
      rownames = FALSE
    )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
