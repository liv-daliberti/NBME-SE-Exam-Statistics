library(ggplot2)

performance_data <- read.csv("performance_data.csv", header=TRUE)

performance_data %>%
  group_by(Group) %>%
  summarize(mean = mean(Score), sd=sd(Score), low= min(Score), high = max(Score), n = n()) ->georgetown_data


national_data$n * national_data$Mean

group_1_mean <- (73.3*19690 + 74.7*19096) /(19690 + 19096)
group_2_mean <- (75.2*19116 + 75.6*19421 + 75.4*19759)/(19116 + 19421 + 19759)

group_1_sd <- sqrt((19689*9**2+ 19095*8.9**2)/(19689+19095))
group_2_sd <- sqrt((19115*8.8**2+ 19420*8.8**2 + 19758*8.9**2)/(19116+19421 +19759 - 3))

group_1_n <- 19690 + 19096
group_2_n <- 19116 + 19421 + 19759

t.test.from.summary.data <- function(mean1, sd1, n1, mean2, sd2, n2, ...) {
  data1 <- scale(1:n1)*sd1 + mean1
  data2 <- scale(1:n2)*sd2 + mean2
  t.test(data1, data2, alternative = "greater")
}
t.test.from.summary.data(77.3, 8.16, 494, 75.4, 8.83, 58296)
