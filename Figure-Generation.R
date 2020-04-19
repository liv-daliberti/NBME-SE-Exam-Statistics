library("ggplot2")
library("dplyr")
library("gridExtra")

#===============================================
# Read in the Georgetown Raw Performance Data 
# as R Dataframe Object
performance_data <- read.csv("performance_data.csv", header=TRUE)

#===============================================
# Generate violin plot of score for 
# Control vs Experimental

p <- ggplot(performance_data, aes(x=Group, y=Score, fill=Group)) + 
  geom_violin() +
  labs(title = "Performance on NMBE Exam", y = "Score", x = "Group") +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  geom_boxplot(width=0.1, fill="white") +
  theme(legend.position="top") +
  scale_fill_discrete(labels=c("Control", "Experimental"))+
  scale_y_continuous(breaks = round(seq(min(performance_data$Score), max(performance_data$Score), by = 10),1))
p

#===============================================
# Generate Q-Q plot of score for 
# Control vs Experimental

control <- as.data.frame(performance_data[which(performance_data$Group == '2014 to 2015'),]$Score)
experimental <- as.data.frame(performance_data[which(performance_data$Group == '2016 to 2018'),]$Score)

p1 <- ggplot(control, aes(sample=performance_data[which(performance_data$Group == "2014 to 2015"), ]$Score))+
  stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Control - 2014-2015")

p2 <- ggplot(experimental, aes(sample=performance_data[which(performance_data$Group == "2016 to 2018"), ]$Score))+
  stat_qq() + 
  stat_qq_line(color = "red")+
  labs(title = "Experimental - 2016-2018")

grid.arrange(p1, p2, top = "Q-Q Plots of Control vs Experimental Data" , nrow=1)

#===============================================
# Generate Q-Q plot of score for 
# Control vs Experimental
# Bottom 5% of performers

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

p1 <- ggplot(bottom_five, aes(sample=Score))+
  stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Control - 2014-2015")

p2 <- ggplot(bottom_five_exp, aes(sample=Score))+
  stat_qq() + 
  stat_qq_line(color = "red")+
  labs(title = "Experimental - 2016-2018")

grid.arrange(p1, p2, top = "Q-Q Plots of Control vs Experimental Bottom 5% of Performers" , nrow=1)

#===============================================
# Generate Q-Q plot of score for 
# Control vs Experimental
# Top 5% of performers

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

p1 <- ggplot(top_five, aes(sample=Score))+
  stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Control - 2014-2015")

p2 <- ggplot(top_five_exp, aes(sample=Score))+
  stat_qq() + 
  stat_qq_line(color = "red")+
  labs(title = "Experimental - 2016-2018")

grid.arrange(p1, p2, top = "Q-Q Plots of Control vs Experimental top 5% of Performers" , nrow=1)





