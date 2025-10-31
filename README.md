# College-to-Draft

College-to-Draft contains NCAA Division I men’s basketball player statistics from the 2024–2025 season, scraped directly from ESPN, and displayed through an interactive R Shiny app.

The intent is to allow users (such as scouts, analysts, and basketball fans) to explore and compare player performance across key statistics and positions in preparation for the 2025 NBA Draft.

Within the app.R file is the full source code for this project, while the included HTML file contains the running application.

## The Player Stats Table

Here one can view a sortable and searchable table of NCAA Division I basketball players, filtered by position (Guard, Forward, or Center) and minimum points per game. This allows users to focus on specific types of players—such as high-scoring guards or rebounding forwards.

## The Player Comparison Bar Chart

Similarly, one can select up to three players to compare across major statistics (Points, Rebounds, Assists, Steals, Blocks, and Turnovers). These comparisons are shown through interactive bar charts, allowing for quick visual insights into each player’s strengths.

## More
### Data Source

Player statistics are obtained directly from ESPN’s NCAA Men’s Basketball Stats
 page.
The dataset reflects the top 50 scorers from the 2024–25 season. Since roughly 60 players are drafted each year, this selection provides a realistic sample of draft-level talent. The data is cleaned and combined automatically within the R Shiny app using the rvest and dplyr packages.

### More R-Based Sports Content

College-to-Draft was developed by Miki Vermeulen, a Data Science and Economics student at Linfield University and a member of the Linfield women’s basketball team.
The project demonstrates the use of R, Shiny, and web scraping for sports data analysis and visualization.
