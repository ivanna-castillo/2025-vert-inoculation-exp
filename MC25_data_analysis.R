##This script includes some data wrangling, analysis, and plot-making for the 2025
##two-factor experiment (inoculation method vs. spore concentration). 


##Data wrangling

#load the libraries
library(agricolae)
library(reshape) #reshape2 does not have the cast function
library(car)
library(dplyr)
library(ggplot2)
library(jpeg)

#set the working directory
setwd("/Users/ikc6/Library/CloudStorage/OneDrive-CornellUniversity/Research_PhD/2025-vert-inoculation-exp/")

#load the data
MC25data <- read.csv("MC_25data3.csv")

#get a look at it 
head(MC25data)

#it is in long format, so we will change it to wide to calculate AUDPC
MC25data_wide <- cast(MC25data, plot ~ date, value = "disease_percentage")
head(MC25data_wide)

#now we want to add the treatment columns 
#first we make a data frame that includes every unique combination of treatments once
treatment <- unique(MC25data[, c("plot", "trt", "inoc_method", "concentration", "trt2")])
#next we combine the the wide df with the treatment data frame
MC25data_wide <- merge(MC25data_wide, treatment, by = "plot", all.x = TRUE)

#now we will fix the order of the columns, so that the treatment columns come first
MC25data_wide <- MC25data_wide[, c("plot", "trt", "trt2", "inoc_method", "concentration", "7/20/25", "7/22/25", "7/24/25", "7/26/25", "7/28/25")]
head(MC25data_wide)

#check how R is reading the data
str(MC25data_wide)

#turn the first five rows into factors to avoid issues
MC25data_wide$plot <- as.factor(MC25data_wide$plot)
MC25data_wide$trt <- as.factor(MC25data_wide$trt)
MC25data_wide$trt2 <- as.factor(MC25data_wide$trt2)
MC25data_wide$inoc_method <- as.factor(MC25data_wide$inoc_method)
MC25data_wide$concentration <- as.factor(MC25data_wide$concentration)
str(MC25data_wide)


##Calculate AUDPC

#make a vector of the dates 
dates <- c("7/20/25", "7/22/25", "7/24/25", "7/26/25", "7/28/25")

#now we make a vector of the days post inoculation by subtracting the inoculation date from each
#date where data was recorded
dpi <- as.Date(dates, format = "%m/%d/%Y") - as.Date("7/10/25", format = "%m/%d/%Y")

#now we make the column with the audpc function from agricolae package
MC25data_wide$audpc <- apply(MC25data_wide[,6:10], 1, audpc, dates = dpi)
head(MC25data_wide)


##Data analysis

#Fit linear model --- AUDPC as a function of entry and block
MC25data_wide.lm <- lm(audpc ~ trt2,  data=MC25data_wide)

#Testing the normality of the data
#test1
qqnorm(scale(resid(MC25data_wide.lm)))
abline(0,1)
#test2
hist(resid(MC25data_wide.lm))
#test3
plot(fitted(MC25data_wide.lm), resid(MC25data_wide.lm))

#Levene's test for homogeneity of variances across groups using car package
leveneTest(audpc ~ trt2, data = MC25data_wide)

#looking at the variance for each treatment combination using dplyr
MC25data_wide %>%
  group_by(trt) %>%
  summarise(variance = var(audpc, na.rm = TRUE))

#Doing a box and whisker plot because Levene's test says that the variances are not equal 
boxplot(MC25data_wide$audpc ~ MC25data_wide$trt2)

#Since levene's test determined unequal variances, we will use a test that does not 
#require equal variances, unlike anova. The test is a one-way, Welch's anova
oneway.test(MC25data_wide$audpc ~ MC25data_wide$trt2)

#Now we want to see which groups are different from one another. This does a 
#Welch's t-test that does not assume equal variance. Every group against every other group
pairwise.t.test(MC25data_wide$audpc, MC25data_wide$trt2, var.equal = FALSE, p.adjust.method = "bonferroni")
hist(resid(MC25data_wide.lm))

##Weighted least squares. When we did the Welch's anova, we were not worried about equal variance
##but as a two-way anova, there is no Welch's two-way anova. This is the solution to that:
wt <- 1 / lm(abs(MC25data_wide.lm$residuals) ~ MC25data_wide.lm$fitted.values)$fitted.values^2

#making the weighted least squares model with both treatments as a single one
wls_model <- lm(audpc ~ trt2, data = MC25data_wide, weights=wt)
anova(wls_model)

#we make a linear model that includes both factors separately
MC25data_wide.lm.both <- lm(audpc ~ concentration * inoc_method,  data=MC25data_wide)
wt <- 1 / lm(abs(MC25data_wide.lm.both$residuals) ~ MC25data_wide.lm.both$fitted.values)$fitted.values^2

#weighted least squares model that includes both factors separately
wls_model <- lm(audpc ~ concentration * inoc_method, data = MC25data_wide, weights=wt)

##This is the trustworthy anova because we are accounting for the different variances
##It takes a weighted average. The ones that are closer to the predicted value get more weight
##than the ones that are farther. 
summary(wls_model)
anova(wls_model)


##Making a basic violin plot 

new_labels <- c("dip_original" = "Original root-dipping", "dipmod" = "Modified root-dipping", "dre" = "Drenching the roots")

violin_plot <- ggplot(MC25data_wide, aes(x = inoc_method, y = audpc, fill = concentration)) +
  geom_violin(trim = FALSE) +
  scale_fill_brewer(palette = "YlOrRd") +
  scale_x_discrete(labels = new_labels) +
  labs(x = "Inoculation Method", y = "AUDPC", fill = "Spore Concentration") +
  theme(axis.title.x = element_text(size = 16), 
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16))
print(violin_plot)

#saving the plot as a jpeg
jpeg(filename = "violin_plot.jpeg", width = 1100, height = 800, units = "px", res = 100)
violin_plot
dev.off()

ggsave("violin_plot2.png", width = 15, height = 7)
