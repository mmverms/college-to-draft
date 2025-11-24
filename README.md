# College-to-NBA Draft Prospect Comparison App

This Shiny app lets you explore NCAA men's basketball stats scraped live from ESPN and compare players across multiple stat categories. You can filter players, view top performers, and even add your own custom players to see how they stack up.

## Features

- **Live ESPN data scraping** using `rvest`
- **Filter players** by position (G, F, C) and minimum points per game
- **Compare up to three players** using an interactive Plotly bar chart
- **Summary of top performers** (points, rebounds, assists, steals, blocks, and lowest turnovers)
- **Add your own custom players** and include them in tables and comparison plots
- **Clean, beginner-friendly UI** using `bslib` and organized tabs
- **Interactive data table** with sortable stats (DT)

## Required Libraries

Make sure these packages are installed:

```r
install.packages(c(
  "rvest", "dplyr", "shiny", "tidyverse", 
  "DT", "bslib", "plotly"
))
```

## How to Run the App

You can run this app on any computer with R + RStudio installed.

* Open RStudio.
* Create a new R script or open your app.R file.
* Copy and paste the entire code from this repository into a single R script (if it isn’t already).
* Make sure all the libraries (listed above) are installed.
* Click Run App in the top right corner of RStudio.

That's it, the app will launch in a Shiny window.

## How It Works

* The app scrapes the ESPN NCAA men’s basketball stats page each time it loads.
* It cleans and reorganizes the scraped table.
* The UI provides filters, selectors, and tabs for:
  * Player stats
  * Bar plot comparisons
  * Summary output
  * Adding custom players
* Custom players are stored reactively for the session.

## Data Source

Data is scraped directly from:

* ESPN NCAA Men's Basketball Stats Page  
  *(updated live each time the app runs)*

## Notes

* Custom players exist only during your Shiny session (reloading resets them).
