# Shiny App used to explore Nutrient Synthesis modeling data set -
# source data is the monthly-regional dataset
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(stringr)
library(shiny)
library(bslib)
library(here)

df_monthly <-
  read_rds(here("data/processed/monthly_values.rds")) |>
  mutate(
    Season = factor(
      case_match(
        Month,
        3:5 ~ "Spring",
        6:8 ~ "Summer",
        9:11 ~ "Fall",
        c(12, 1, 2) ~ "Winter"
      ),
      levels = c("Winter", "Spring", "Summer", "Fall")
    ),
    Date = make_date(year = Year, month = Month),
    Month = month(Month, label = TRUE),
    Region = factor(
      Region,
      levels = c(
        "North",
        "Confluence",
        "SouthCentral",
        "Suisun Marsh",
        "Suisun Bay"
      )
    )
  )

plot_vars <- df_monthly |>
  select(where(is.numeric) & !all_of(c("Year", "DissNitrate"))) |>
  names()

plot_vars_box <- str_subset(plot_vars, "_(Inflow|Index)$", negate = TRUE)

ui <- page_navbar(
  title = "Nutrient Synthesis Data Explorer",
  id = "nav",
  sidebar = sidebar(
    title = "Plot Controls",
    conditionalPanel(
      condition = "input.nav == 'xy_plot'",
      selectInput("scatt_x", "X variable", choices = plot_vars),
      selectInput("scatt_y", "Y variable", choices = plot_vars),
      radioButtons(
        "facet_opt",
        "Facet Options",
        choices = c(
          "none",
          "Region",
          "Month",
          "Season",
          "Region and Month",
          "Region and Season"
        ),
        selected = "Region"
      )
    ),
    conditionalPanel(
      condition = "input.nav == 'ts_plot'",
      selectInput("ts_y", "Y variable", choices = plot_vars)
    ),
    conditionalPanel(
      condition = "input.nav == 'boxplot'",
      radioButtons(
        "box_x",
        "X variable",
        choices = c(
          "Region",
          "Month",
          "Season",
          "Region and Month",
          "Region and Season"
        ),
        selected = "Region"
      ),
      selectInput("box_y", "Y variable", choices = plot_vars_box)
    ),
    width = 300
  ),
  nav_panel(
    title = "X-Y Scatterplots",
    value = "xy_plot",
    plotOutput("plot_xy")
  ),
  nav_panel(
    title = "Time-series Plots",
    value = "ts_plot",
    plotOutput("plot_ts")
  ),
  nav_panel(
    title = "Boxplots",
    value = "boxplot",
    plotOutput("plot_box")
  )
)

server <- function(input, output, session) {
  output$plot_xy <- renderPlot(
    {
      xy_plt_base <- df_monthly |>
        ggplot(
          aes(
            x = .data[[input$scatt_x]],
            y = .data[[input$scatt_y]],
            color = Region
          )
        ) +
        geom_point(na.rm = TRUE, alpha = 0.7) +
        theme_bw()

      switch(
        input$facet_opt,
        none = xy_plt_base,
        Region = xy_plt_base + facet_wrap(vars(Region), scales = "free"),
        Month = xy_plt_base + facet_wrap(vars(Month), scales = "free"),
        Season = xy_plt_base + facet_wrap(vars(Season), scales = "free"),
        `Region and Month` = xy_plt_base +
          facet_grid(rows = vars(Region), cols = vars(Month), scales = "free"),
        `Region and Season` = xy_plt_base +
          facet_grid(rows = vars(Region), cols = vars(Season), scales = "free")
      )
    },
    res = 96,
    width = function() {
      switch(
        input$facet_opt,
        none = 650,
        Region = 950,
        Month = 1000,
        Season = 775,
        `Region and Month` = 1300,
        `Region and Season` = 850
      )
    },
    height = function() {
      switch(
        input$facet_opt,
        none = 500,
        Region = 550,
        Month = 600,
        Season = 600,
        `Region and Month` = 675,
        `Region and Season` = 775
      )
    }
  )

  output$plot_ts <- renderPlot(
    {
      df_monthly |>
        ggplot(aes(x = Date, y = .data[[input$ts_y]])) +
        geom_point(na.rm = TRUE, alpha = 0.6) +
        theme_bw() +
        facet_grid(rows = vars(Region), scales = "free") +
        scale_x_date(date_breaks = "year", date_labels = "%Y")
    },
    res = 96,
    width = 900,
    height = 750
  )

  output$plot_box <- renderPlot(
    {
      boxplt_base <- df_monthly |>
        ggplot(aes(y = .data[[input$box_y]])) +
        geom_boxplot(na.rm = TRUE) +
        theme_bw()

      switch(
        input$box_x,
        Region = boxplt_base + aes(x = Region),
        Month = boxplt_base + aes(x = Month),
        Season = boxplt_base + aes(x = Season),
        `Region and Month` = boxplt_base +
          aes(x = Month) +
          facet_grid(rows = vars(Region), scales = "free"),
        `Region and Season` = boxplt_base + aes(x = Region, fill = Season)
      )
    },
    res = 96,
    width = 800,
    height = function() {
      ifelse(input$box_x == "Region and Month", 700, 600)
    }
  )
}

shinyApp(ui, server)
