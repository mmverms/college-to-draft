# Install/load libraries
library(rvest)
library(dplyr)
library(shiny)
library(tidyverse)
library(readxl)
library(DT)
library(bslib)


# Learned this through research and ChatGPT
# Scrape the ESPN NCAA men's basketball stats page
page <- read_html("https://www.espn.com/mens-college-basketball/stats/player")

# Extract all tables
tables <- page %>% html_table(fill = TRUE)

# Combine the two scraped tables into one dataset
players <- bind_cols(tables[[1]], tables[[2]])

# Clean up the Name column (the names have school abbreviations stuck to them)
players <- players %>%
  mutate(
    # Remove the team abbreviation (all capital letters at the end)
    Name = gsub("([A-Z]{2,})$", "", Name),
    
    # Trim extra spaces left after removing the team abbreviation
    Name = trimws(Name)
  )

# Check the result
head(players)
nrow(players)

# Remove the rank column
players <- players %>% select(-RK)


# Define UI
ui <- fluidPage(
  titlePanel("NBA Draft Prospect Comparison App"),
  
  p("Use the checkboxes and dropdown menus to select player positions and compare stats across players."),
  
  sidebarLayout(
    sidebarPanel(
      # Position filter
      checkboxGroupInput(
        inputId = "selected_player_pos", 
        label = "Select Player Position:", 
        choices = c("Guard" = "G", "Forward" = "F", "Center" = "C"),
        selected = unique(players$POS)
      ),
      
      # Slider to filter by minimum points per game
      sliderInput(
        inputId = "min_ppg",
        label = "Minimum Points Per Game:",
        min = 0,
        max = 30,
        value = 0,
        step = 1
      ),
      
      # Optional bar plot checkbox
      checkboxInput(
        inputId = "show_barplot",
        label = "Display the bar plot below",
        value = TRUE
      ),
      
      # Player selectors
      selectInput("player1", "Select Player 1:", choices = players$Name),
      selectInput("player2", "Select Player 2:", choices = players$Name),
      selectInput("player3", "Select Player 3:", choices = players$Name)
    ),
    
    mainPanel( # br is for a line break
      # Player stats table output
      dataTableOutput("players_table"),
      
      br(),
      # Bar plot for stat comparisons
      plotOutput("barplot", height = "600px"),
      
      br(),
      p("Note: Stats shown are per game averages from ESPN's NCAA player data.")
    )
  )
)


# Define server
server <- function(input, output, session) {
  
  # Filter players by position and points per game
  filtered_players <- reactive({
    players %>%
      filter(
        POS %in% input$selected_player_pos,   # filter by selected position(s)
        PTS >= input$min_ppg                 # apply min points per game filter
      )
  })
  
  # Get selected players for comparison
  selected_players <- reactive({
    filtered_players() %>%
      filter(Name %in% c(input$player1, input$player2, input$player3))
  })
  
  # Create the bar plot
  output$barplot <- renderPlot({
    if (input$show_barplot) {
      df <- selected_players() %>%
        select(Name, PTS, REB, AST, STL, BLK, TO) %>%   # pick main stats
        rename(
          Points = PTS,
          Rebounds = REB,
          Assists = AST,
          Steals = STL,
          Blocks = BLK,
          Turnovers = TO
        ) %>%
        pivot_longer(-Name, names_to = "Stat", values_to = "Value")
      
      ggplot(df, aes(x = Stat, y = Value, fill = Name)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(y = "Value", x = "Stat", title = "Player Stat Comparison") +
        theme_minimal(base_size = 14)
    }
  })
  
  # Data table output
  output$players_table <- renderDataTable({
    DT::datatable(
      data = filtered_players() %>%
        arrange(desc(PTS)),   # sort by points per game
      options = list(pageLength = 10),
      rownames = FALSE
    )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
