library("ggplot2")
library("dplyr")

#===============================================
# Read in the Georgetown Raw Performance Data 
# as R Dataframe Object
performance_data <- read.csv("performance_data.csv", header=TRUE)

performance_data %>%
  group_by(Year) %>%
  summarize(mean = mean(Score), sd=sd(Score), low= min(Score), high = max(Score), n = n())

performance_data %>%
  group_by(Group) %>%
  summarize(mean = mean(Score), sd=sd(Score), low= min(Score), high = max(Score), n = n())

#===============================================
# Welches Two Sample t-test
t.test(performance_data[which(performance_data$Group == '2014 to 2015'),]$Score,
       performance_data[which(performance_data$Group == '2016 to 2018'),]$Score)

#===============================================
# Shapiro wilks normaility

shapiro.test(performance_data[which(performance_data$Group == '2014 to 2015'),]$Score)
shapiro.test(performance_data[which(performance_data$Group == '2016 to 2018'),]$Score)

#===============================================
# Wilcox Test

wilcox.test(performance_data[which(performance_data$Group == '2014 to 2015'),]$Score,
            performance_data[which(performance_data$Group == '2016 to 2018'),]$Score, 
            alternative = "two.sided")

#===============================================
# Variance F-test
var.test(Score ~ Group, data = performance_data)

#===============================================
# Bottom 5% of scores

bottom_five <- as.data.frame(sort(performance_data[which(performance_data$Group == '2014 to 2015'),]$Score)[1:19])
bottom_five$group <- "Control"
colnames(bottom_five) <- c("Score", "Group")

bottom_five_exp <- as.data.frame(sort(performance_data[which(performance_data$Group == '2016 to 2018'),]$Score)[1:25])
bottom_five_exp$group <- "Experimental"
colnames(bottom_five_exp) <- c("Score", "Group")

bottom_five_together <- rbind(bottom_five, bottom_five_exp)

bottom_five_together %>%
  group_by(Group) %>%
  summarize(mean = mean(Score),median(Score), sd=sd(Score), low= min(Score), high = max(Score), n = n())

#===============================================
# Variance F-test
var.test(Score ~ Group, data = bottom_five_together)

#===============================================
# Shapiro wilks normaility

shapiro.test(bottom_five$Score)
shapiro.test(bottom_five_exp$Score)

#===============================================
# Wilcox Test

wilcox.test(Score ~ Group, data= bottom_five_together,
       alternative="greater", exact=FALSE)

#===============================================
# Top 5% of scores

top_five <- tail(as.data.frame(sort(performance_data[which(performance_data$Group == '2014 to 2015'),]$Score)),19)
top_five$group <- "Control"
colnames(top_five) <- c("Score", "Group")

top_five_exp <- tail(as.data.frame(sort(performance_data[which(performance_data$Group == '2016 to 2018'),]$Score)),25)
top_five_exp$group <- "Experimental"
colnames(top_five_exp) <- c("Score", "Group")

top_five_together <- rbind(top_five, top_five_exp)

top_five_together %>%
  group_by(Group) %>%
  summarize(mean = mean(Score),median(Score), sd=sd(Score), low= min(Score), high = max(Score), n = n())

#===============================================
# Variance F-test
var.test(Score ~ Group, data = top_five_together)

#===============================================
# Shapiro wilks normaility

shapiro.test(top_five$Score)
shapiro.test(top_five_exp$Score)

#===============================================
# Wilcox Test

wilcox.test(Score ~ Group, data= top_five_together,
            alternative="two.sided", exact=FALSE)





     