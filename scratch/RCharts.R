### Explore
library(dplyr)
library(reshape2)
library(ggplot2)
library(rCharts)

dat <- read.csv("Data/2014_Cleaned.csv")

datM <- dat[,-13] %>% melt(., 1:4, variable.name = "mode", value.name = "count")

# modes by year and school type
modes <- datM %>% 
  group_by(School.Type, mode, Year) %>% 
  summarise(count = sum(count, na.rm = T)) %>%
  filter(!is.na(School.Type))

tfrPlot <- nPlot(
  count ~ Year, 
  data = modes, 
  group = "School.Type",
  type = "lineChart")

tfrPlot$yAxis(axisLabel = "Count", width = 62)
tfrPlot$xAxis(axisLabel = "Year")
tfrPlot$chart(tooltipContent = "#! function(key, x, y){
              return '<h3>' + key + '</h3>' + 
              '<p>' + y + ' in ' + x + '</p>'
              } !#")


# 
# # Create the chart
# tfrPlot <- nPlot(
#   TFR ~ year, 
#   data = tfr, 
#   group = "country",
#   type = "lineChart")
# 
# # Add axis labels and format the tooltip
# tfrPlot$yAxis(axisLabel = "Total fertility rate", width = 62)
# 
# tfrPlot$xAxis(axisLabel = "Year")
# 
# tfrPlot$chart(tooltipContent = "#! function(key, x, y){
#               return '<h3>' + key + '</h3>' + 
#               '<p>' + y + ' in ' + x + '</p>'
#               } !#")