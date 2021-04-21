library(ggplot2)
library(car)
library(dplyr)
library(ggpubr)
library(devtools)
library(outliers)
#Store data
decomp=read.csv("data/Copy of decomposition Data.csv")
#Make weeks factors
decomp$Time_weeks = as.factor(decomp$Time_weeks)
#test for equal variance
leveneTest(Fraction.remained ~ Inoculation.Amount*Time_weeks, data=decomp)
#Variance equal, anova is fine
decomp.aov=aov(Fraction.remained ~ Inoculation.Amount*Time_weeks, data=decomp)
summary(decomp.aov)
#treatment not significant, time is. No relation between the two

#make mean data

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

df3=data_summary(decomp, varname="Fraction.remained",
                 groupnames=c("Inoculation.Amount", "Time_weeks"))

#make lm function
eq <- function(x,y) {
  m <- lm(y ~ x)
  as.character(
    as.expression(
      substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                 list(a = format(coef(m)[1], digits = 4),
                      b = format(coef(m)[2], digits = 4),
                      r2 = format(summary(m)$r.squared, digits = 3)))
    )
  )
}

ggplot(df3, aes(x=Time_weeks, y=Fraction.remained, group=Inoculation.Amount))+
  geom_line(aes(color=Inoculation.Amount))+
  geom_point(aes(shape=Inoculation.Amount))+
  ylab("Remaining Fraction of Paper")+
  xlab("Time (in weeks)")+
  ggtitle("Effect of Kombucha Microbiome Population on Paper Degradation over Time")+
  scale_color_brewer(palette="YlGn")
  


ggplot(decomp, aes(x=Time_weeks, y=Fraction.remained, fill=Inoculation.Amount))+
  geom_bar(stat="identity", position=position_dodge())+
  ylab("Remaining Fraction of Paper")+
  xlab("Time (in weeks)")+
  ggtitle("Effect of Kombucha Microbiome Population on Paper Degradation over Time")+
  scale_fill_brewer(palette="YlGn")+
  facet_wrap(~Inoculation.Amount)+
  geom_smooth(data=decomp, aes(x=Time_weeks, y = Fraction.remained, group=1),
              method = "lm", se= FALSE, color = "chocolate4", size = 1)+
  geom_point(stat="identity", position=position_dodge(width=.9))+
  stat_regline_equation(aes(x=Time_weeks, y=Fraction.remained, label.y=2))
  
grubbs.test(decomp$Fraction.remained)
grubbs.test(decomp$Fraction.remained, opposite = TRUE)

