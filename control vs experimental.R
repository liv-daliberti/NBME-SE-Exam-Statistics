library(ggplot2)

performance_data <- read.csv("performance_data.csv", header=TRUE)

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

ggplot(performance_data, aes(x=Score, fill=Group))
  geom_bar(bins=3)

library(dplyr)

performance_data %>%
  group_by(Year) %>%
  summarize(mean = mean(Score), sd=sd(Score), low= min(Score), high = max(Score), n = n())

performance_data %>%
  group_by(Group) %>%
  summarize(mean = mean(Score), sd=sd(Score), low= min(Score), high = max(Score), n = n())

performance_data %>%
  group_by(Group)

## Welches Two Sample t-test
t.test(performance_data[which(performance_data$Group == '2014 to 2015'),]$Score,
       performance_data[which(performance_data$Group == '2016 to 2018'),]$Score)


#shapiro wilks normaility
shapiro.test(performance_data[which(performance_data$Group == '2014 to 2015'),]$Score)
shapiro.test(performance_data[which(performance_data$Group == '2016 to 2018'),]$Score)
library("gridExtra")

control <- as.data.frame(performance_data[which(performance_data$Group == '2014 to 2015'),]$Score)
experimental <- as.data.frame(performance_data[which(performance_data$Group == '2016 to 2018'),]$Score)
control
p1 <- ggplot(control, aes(sample=performance_data[which(performance_data$Group == "2014 to 2015"), ]$Score))+
  stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Control - 2014-2015")

p2 <- ggplot(experimental, aes(sample=performance_data[which(performance_data$Group == "2016 to 2018"), ]$Score))+
  stat_qq() + 
  stat_qq_line(color = "red")+
  labs(title = "Experimental - 2016-2018")

grid.arrange(p1, p2, top = "Q-Q Plots of Control vs Experimental Data" , nrow=1)


#Wilcox Test

wilcox.test(performance_data[which(performance_data$Group == '2014 to 2015'),]$Score,
            performance_data[which(performance_data$Group == '2016 to 2018'),]$Score, 
            alternative = "two.sided")


#Variance F-test
var.test(Score ~ Group, data = performance_data)

## Bottom 5% of scores

bottom_ten <- as.data.frame(sort(performance_data[which(performance_data$Group == '2014 to 2015'),]$Score)[1:19])
bottom_ten$group <- "Control"
colnames(bottom_ten) <- c("Score", "Group")

bottom_ten_exp <- as.data.frame(sort(performance_data[which(performance_data$Group == '2016 to 2018'),]$Score)[1:25])
bottom_ten_exp$group <- "Experimental"
colnames(bottom_ten_exp) <- c("Score", "Group")
bottom_ten_exp
bottom_ten_together <- rbind(bottom_ten, bottom_ten_exp)

bottom_ten_together %>%
  group_by(Group) %>%
  summarize(mean = mean(Score),median(Score), sd=sd(Score), low= min(Score), high = max(Score), n = n())


var.test(Score ~ Group, data = bottom_ten_together)



p1 <- ggplot(bottom_ten, aes(sample=Score))+
  stat_qq() + 
  stat_qq_line(color = "red") +
  labs(title = "Control - 2014-2015")

p2 <- ggplot(bottom_ten_exp, aes(sample=Score))+
  stat_qq() + 
  stat_qq_line(color = "red")+
  labs(title = "Experimental - 2016-2018")
grid.arrange(p1, p2, top = "Q-Q Plots of Control vs Experimental Data Top 5% of Performers" , nrow=1)



wilcox.test(Score ~ Group, data= bottom_ten_together,
       alternative="two.sided", exact=FALSE)
wilcox.test(bottom_ten$Score, bottom_ten_exp$Score, alternative="two.sided", exact=FALSE)

shapiro.test(bottom_ten$Score)
shapiro.test(bottom_ten_exp$Score)

library(ggpubr)

ggqqplot(performance_data[which(performance_data$Group == '2016 to 2018'),]$Score)

shapiro.test(performance_data[which(performance_data$Group == '2016 to 2018'),]$Score)

maxN <- function(x, N=2){
  len <- length(x)
  if(N>len){
    warning('N greater than length(x).  Setting N=length(x)')
    N <- length(x)
  }
  sort(x,partial=len-N+1)[len-N+1]
}


## Lowest ten scores
t.test(sort(performance_data[which(performance_data$Group == '2014 to 2015'),]$Score)[1:10],
       sort(performance_data[which(performance_data$Group == '2016 to 2018'),]$Score)[1:10],
       alternative = "less", conf.level=0.05)


performance_data[which(performance_data$Group == '2014 to 2015'),]$Score
     