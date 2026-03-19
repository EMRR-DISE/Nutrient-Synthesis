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
      radioButtons(
        "scatt_x_log",
        "Log-transform X variable?",
        choices = c("No", "Yes"),
        inline = TRUE
      ),
      selectInput("scatt_y", "Y variable", choices = plot_vars),
      radioButtons(
        "scatt_y_log",
        "Log-transform Y variable?",
        choices = c("No", "Yes"),
        inline = TRUE
      ),
      radioButtons(
        "scatt_facet_opt",
        "Facet Options",
        choices = c(
          "None" = "none",
          "Region" = "region",
          "Month" = "month",
          "Season" = "season",
          "Region and Month" = "region_month",
          "Region and Season" = "region_season"
        ),
        selected = "region"
      ),
      radioButtons(
        "scatt_trendline",
        "Add a linear trend-line?",
        choices = c("No", "Yes"),
        inline = TRUE
      )
    ),
    conditionalPanel(
      condition = "input.nav == 'ts_plot'",
      selectInput("ts_y", "Y variable", choices = plot_vars),
      radioButtons(
        "ts_y_log",
        "Log-transform Y variable?",
        choices = c("No", "Yes"),
        inline = TRUE
      )
    ),
    conditionalPanel(
      condition = "input.nav == 'boxplot'",
      radioButtons(
        "box_x",
        "X variable",
        choices = c(
          "Region" = "region",
          "Month" = "month",
          "Season" = "season",
          "Region and Month" = "region_month",
          "Region and Season" = "region_season"
        ),
        selected = "region"
      ),
      selectInput("box_y", "Y variable", choices = plot_vars_box),
      radioButtons(
        "box_y_log",
        "Log-transform Y variable?",
        choices = c("No", "Yes"),
        inline = TRUE
      )
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
        ggplot(aes(color = Region)) +
        geom_point(na.rm = TRUE, alpha = 0.7) +
        theme_bw()

      xy_plt_base <- switch(
        input$scatt_x_log,
        No = xy_plt_base + aes(x = .data[[input$scatt_x]]),
        Yes = xy_plt_base + aes(x = log(.data[[input$scatt_x]]))
      )

      xy_plt_base <- switch(
        input$scatt_y_log,
        No = xy_plt_base + aes(y = .data[[input$scatt_y]]),
        Yes = xy_plt_base + aes(y = log(.data[[input$scatt_y]]))
      )

      xy_plt_base <- switch(
        input$scatt_trendline,
        No = xy_plt_base,
        Yes = xy_plt_base +
          geom_smooth(na.rm = TRUE, method = "lm", formula = "y ~ x")
      )

      switch(
        input$scatt_facet_opt,
        none = xy_plt_base,
        region = xy_plt_base + facet_wrap(vars(Region), scales = "free"),
        month = xy_plt_base + facet_wrap(vars(Month), scales = "free"),
        season = xy_plt_base + facet_wrap(vars(Season), scales = "free"),
        region_month = xy_plt_base +
          facet_grid(rows = vars(Region), cols = vars(Month), scales = "free"),
        region_season = xy_plt_base +
          facet_grid(rows = vars(Region), cols = vars(Season), scales = "free")
      )
    },
    res = 96,
    width = function() {
      switch(
        input$scatt_facet_opt,
        none = 650,
        region = 950,
        month = 1000,
        season = 775,
        region_month = 1300,
        region_season = 850
      )
    },
    height = function() {
      switch(
        input$scatt_facet_opt,
        none = 500,
        region = 550,
        month = 600,
        season = 600,
        region_month = 675,
        region_season = 775
      )
    }
  )

  output$plot_ts <- renderPlot(
    {
      ts_plt_base <- df_monthly |>
        ggplot(aes(x = Date)) +
        geom_point(na.rm = TRUE, alpha = 0.6) +
        geom_line(na.rm = TRUE) +
        theme_bw() +
        facet_grid(rows = vars(Region), scales = "free") +
        scale_x_date(date_breaks = "year", date_labels = "%Y")

      switch(
        input$ts_y_log,
        No = ts_plt_base + aes(y = .data[[input$ts_y]]),
        Yes = ts_plt_base + aes(y = log(.data[[input$ts_y]]))
      )
    },
    res = 96,
    width = 900,
    height = 750
  )

  output$plot_box <- renderPlot(
    {
      boxplt_base <- df_monthly |>
        ggplot() +
        geom_boxplot(na.rm = TRUE) +
        theme_bw()

      boxplt_base <- switch(
        input$box_y_log,
        No = boxplt_base + aes(y = .data[[input$box_y]]),
        Yes = boxplt_base + aes(y = log(.data[[input$box_y]]))
      )

      switch(
        input$box_x,
        region = boxplt_base + aes(x = Region),
        month = boxplt_base + aes(x = Month),
        season = boxplt_base + aes(x = Season),
        region_month = boxplt_base +
          aes(x = Month) +
          facet_grid(rows = vars(Region), scales = "free"),
        region_season = boxplt_base + aes(x = Region, fill = Season)
      )
    },
    res = 96,
    width = 800,
    height = function() {
      ifelse(input$box_x == "region_month", 700, 600)
    }
  )
}

shinyApp(ui, server)
