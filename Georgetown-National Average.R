library("ggplot2")
library("dplyr")

#===============================================
# Read in the Georgetown Raw Performance Data 
# as R Dataframe Object
performance_data <- read.csv("performance_data.csv", header=TRUE)

#===============================================
# Use Dplyr Piping to generate summary stats 
# for Control vs Experimental

performance_data %>%
  group_by(Group) %>%
  summarize(mean = mean(Score), sd=sd(Score), low= min(Score), high = max(Score), n = n()) ->georgetown_data

georgetown_data

#===============================================
# Use Dplyr Piping to generate summary stats 
# for each year

performance_data %>%
  group_by(Year) %>%
  summarize(mean = mean(Score), sd=sd(Score), low= min(Score), high = max(Score), n = n()) ->georgetown_data_year

georgetown_data_year


#===============================================
## Add the national Average Data

group_1_mean <- (73.3*19690 + 74.7*19096) /(19690 + 19096)
group_2_mean <- (75.2*19116 + 75.6*19421 + 75.4*19759)/(19116 + 19421 + 19759)

group_1_sd <- sqrt((19689*9**2+ 19095*8.9**2)/(19689+19095))
group_2_sd <- sqrt((19115*8.8**2+ 19420*8.8**2 + 19758*8.9**2)/(19116+19421 +19759 - 3))

group_1_n <- 19690 + 19096
group_2_n <- 19116 + 19421 + 19759

group_1 <- c(group_1_mean, group_1_sd, NA, NA, group_1_n)
group_2 <- c(group_2_mean, group_2_sd, NA, NA, group_2_n)

national_averages <- rbind(group_1, group_2)
national_averages

#===============================================
# Use Welches T-Test to compare Georgetown 
# mean vs National mean in experimental phase
# and in control phase

t.test.from.summary.data <- function(mean1, sd1, n1, mean2, sd2, n2, ...) {
  data1 <- scale(1:n1)*sd1 + mean1
  data2 <- scale(1:n2)*sd2 + mean2
  t.test(data1, data2, alternative = "greater")
}


# control
t.test.from.summary.data(georgetown_data[which(georgetown_data$Group == '2014 to 2015'),]$mean,
                         georgetown_data[which(georgetown_data$Group == '2014 to 2015'),]$sd, 
                         georgetown_data[which(georgetown_data$Group == '2014 to 2015'),]$n,
                         group_1_mean,
                         group_1_sd,
                         group_1_n)

# experimental
t.test.from.summary.data(georgetown_data[which(georgetown_data$Group == '2016 to 2018'),]$mean,
                         georgetown_data[which(georgetown_data$Group == '2016 to 2018'),]$sd, 
                         georgetown_data[which(georgetown_data$Group == '2016 to 2018'),]$n,
                         group_2_mean,
                         group_2_sd,
                         group_2_n)

