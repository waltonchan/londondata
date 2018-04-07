library(ggplot2)
library(ggmap)
library(rgdal)
library(tmap)

#cartogoraphy
bor <- read.csv("C:\\Users\\walto\\Desktop\\DataForStudents.csv")
ldn <- readOGR(dsn = "C:\\Users\\walto\\Downloads\\boundaries\\MapInfo\\London_Borough_Excluding_MHW.tab")
ldndata <- merge(x = ldn, y = bor, by.x = "Name", by.y = "Area")

qtm(ldndata, fill = "PopDens", text = "Name", text.size = 0.7, title = "Illustration of Population Density of London Boroughs") 

qtm(ldndata, fill = "AverageAge", text = "Name", text.soze = 0.7, title = "Illustration of Average Age Across London Boroughs")

summary.data.frame(bor)

#relationship of two variables (Does male pay affect life expectancy?)
mean(bor$MaleLifeExpectancy, na.rm = TRUE)

ggplot(data = bor, mapping = aes(x = bor$MalePay, y = bor$MaleLifeExpectancy)) +
  geom_point(aes(x = bor$MalePay, y = bor$MaleLifeExpectancy)) +
  geom_smooth(method = "lm", se = FALSE)

lm <- lm(bor$MaleLifeExpectancy ~ bor$MalePay, data = bor)
summary(lm) #p-value is <0.05, null hypothesis is rejected

#t-tests: is the mean of life expectacy in outer london = mean of life expectancy in inner london
ggboxplot(bor, x = "InnerOuter", y = "AverageAge", xlab = "Inner Or Outer London", ylab = "Average Age", title = "Average Population Age of Inner and Outer London")

inner <- subset(bor, InnerOuter == "Inner London", select = c(AverageAge))
outer <- subset(bor, InnerOuter == "Outer London", select = c(AverageAge))
t.test(x = inner, y = outer) #p value > 0.05, the null hypothesis cannot be rejected 


