install.packages("tidyverse")
install.packages("tidymodels")
install.packages("skimr")
install.packages("DataExplorer")
install.packages("GGally")

library(tidyverse)
library(tidymodels)
library(vroom)
library(dplyr)
library(skimr)
library(DataExplorer)
library(GGally)
library(ggplot)
library(ggplot2)

?vroom

testData <- vroom("test.csv")
View(testData)


ggplot(testData, aes(x = datetime, y = windspeed)) +
  geom_point()

var(testData$weather)


glimpse(testData)
skimr::skim(testData)
DataExplorer::plot_intro(testData)
DataExplorer::plot_correlation(testData)
DataExplorer::plot_bar(testData)
DataExplorer::plot_histrograms(testData) #doesn't work??? not sure why not
DataExplorer::plot_missing(testData) #nothing missing! Great
GGally::ggpairs(testData)


ggplot(testData, aes(x = weather, y = holiday)) +
  geom_bar(stat = "identity")

plot1 <- ggplot(testData, aes(x = weather, y = temp)) +
  stat_summary(fun.data = mean_sdl, geom = "bar")

plot2 <- ggplot(data = testData, aes(season, temp)) +
  stat_summary(fun.data = mean_sdl, geom = "bar")

plot3 <- ggplot(testData, aes(x = datetime, y = windspeed)) +
  geom_point()

plot4 <- ggplot(testData, aes(x = temp, y = windspeed)) +
  stat_summary(fun.data = mean_sdl, geom = "point") +
  labs(y = "average windspeed")

?ggsave
library(patchwork)

patch_plot <- (plot1 + plot2) / (plot3 + plot4) #4 panel plot
ggsave(filename = "test_data_patch_plot.png", plot = patch_plot)
