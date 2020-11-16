library(echarts4r)

e_common(font_family = "Questrial")

# counties object should be passed as a distinct character vector
echartBarUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    echarts4rOutput(ns("chart"))
  )
  
}

echartBarServer <- function(input, output, session, df, column_name, chart_name) {
  
  output$chart <- renderEcharts4r({
    new_dat <- df() %>%
      rename(series = all_of(column_name))
    
    new_dat %>%
      e_charts(date) %>%
      e_bar(serie = series, name = chart_name, smooth = TRUE, showSymbol = FALSE) %>%
      e_tooltip(trigger = "axis") %>%
      e_x_axis(splitLine = FALSE) %>%
      e_legend(show = FALSE) %>%
      e_color(color = "#4B9CD3") %>%
      e_title(text = chart_name, 
              subtext = sprintf("%s County, North Carolina", unique(new_dat$Admin2))) %>%
      e_toolbox_feature(feature = "saveAsImage", title = "Download as Image")
  })
  
}