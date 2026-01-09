#Line plot of disease progression for two-factor experiment on inoculation methods

library(dplyr)
library(ggplot2)
library(patchwork)

#for using ggplot we need the data in long format
#load the data
MC25data <- read.csv("MC_25data3.csv")

#creating a column with dpi
MC25data$dpi <- as.Date(MC25data$date, format = "%m/%d/%Y") - as.Date("7/10/25", format = "%m/%d/%Y")

#making trt a factor
MC25data$trt <- as.factor(MC25data$trt)

#changing the names of the factor levels for our legend
MC25data$trt <- factor(MC25data$trt, 
                       levels = c("dip_original_10_5", "dip_original_10_6", "dip_original_10_7", "dipmod_10_5",
                                  "dipmod_10_6", "dipmod_10_7", "dre_10_5", "dre_10_6",
                                  "dre_10_7"),
                       labels = c("Original root-dipping x 10^5 spores/mL", 
                                  "Original root-dipping x 10^6 spores/mL",
                                  "Original root-dipping x 10^7 spores/mL",
                                  "Modified root-dipping x 10^5 spores/mL",
                                  "Modified root-dipping x 10^6 spores/mL",
                                  "Modified root-dipping x 10^7 spores/mL", 
                                  "Root drenching x 10^5 spores/mL",
                                  "Root drenching x 10^6 spores/mL",
                                  "Root drenching x 10^7 spores/mL"))
                       
#making the plot with ggplot, aes makes the grid and it is where we determine the looks of the graph
plot3 <- MC25data %>%
  ggplot(aes(x = dpi, y = disease_percentage, color = trt, group = trt)) + #ggplot does the graph as multiple layers
  stat_summary(geom = "point", position = "dodge") + #geom point plots every single data point
  stat_summary(geom = "line", position = "dodge") + #stat_summary adds a layer with a summary statistic of the data, not the raw data
  stat_summary(geom = "errorbar", position = "dodge") +
  labs(x = "DPI", y = "% Chlorosis", title = "Spore Concentration vs. Inoculation Method", color = "")

print(plot3)