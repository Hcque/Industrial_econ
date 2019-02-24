
##1##########################################################
setwd("G:/R/10.16")
m <- read.csv("5.csv")
#m2 <- m[c(2,3,4),c(-1)] 
#m3 <- ts(m)
m4 <- t(m)
m4 <- as.data.frame(m4)
years = c(1975,1980,1985,1990,seq(from = 1995, to = 2015))
m5 <- cbind(m4, years)
m6 <- as.data.frame(m5)
library(ggplot2)
ggplot(m6, aes(years, main = "内蒙产业比重变化")) + 
  geom_line(aes(y = m6$V1), colour = "blue", lty = 2, lwd = 1) +
  geom_line(aes(y = m6$V2), colour = "red", lty = 6, lwd = 1)  +
  geom_line(aes(y = m6$V3), colour = "purple", lwd = 1) 

ggplot(m6, aes(years, V1)) + 
  geom_line(aes(y = m6$V1), colour = "blue", lty = 2, lwd = 1) +
  geom_line(aes(y = m6$V2), colour = "red", lty = 6, lwd = 1)  +
  geom_line(aes(y = m6$V3), colour = "purple", lwd = 1) 



p <- ggplot(m6, aes(years))
p + stat_bin(aes(size = ..density..),binwidth = 0.1,
             geom = "point", position = "identity")



# setwd("G:/R/10.16")
m <- read.csv("6.csv")
m2 <- m[c(-4),] 
#m3 <- ts(m)
m4 <- t(m2)
m4 <- as.data.frame(m4)

years <- c(1980, 1985, 1990, 1995:1999)
m5 <- cbind(m4, years)
library(ggplot2)

ggplot(m5, aes(years, main = "内蒙产业比重变化")) + 
  geom_line(aes(y = m5$`6`), colour = 1, lty = 2, lwd = 1)
 

library("Rcmdr")
attach(m5)
scatter3d(2, 3, 4)

library("rgl", lib.loc="C:/Program Files/R/R-3.4.1/library")
attach(m5)
plot3d(2, 3, 4, col = 2, size = 5)


# setwd("G:/R/10.16")
m <- read.csv("2.csv")
#m2 <- m[c(2,3,4),c(-1)] 
#m3 <- ts(m)
m4 <- t(m)
m4 <- as.data.frame(m4)
years = c(1975,1980,1985,1990,seq(from = 1995, to = 2015))
m5 <- cbind(m4, years)
m6 <- as.data.frame(m5)
library(ggplot2)
ggplot(m6, aes(years, main = "内蒙就业比重变化")) + 
  geom_line(aes(y = m6$V1), colour = "blue", lty = 2, lwd = 1) +
  geom_line(aes(y = m6$V2), colour = "red", lty = 6, lwd = 1)  +
  geom_line(aes(y = m6$V3), colour = "purple", lwd = 1) 


# setwd("G:/R/10.16")
m <- read.csv("3.csv")
m4 <- t(m)
m4 <- as.data.frame(m4)
years = c(1975,1980,1985,1990,seq(from = 1995, to = 2015))
m5 <- cbind(m4, years)
m6 <- as.data.frame(m5)
library("Rcmdr")
attach(m6)
scatter3d(V1, V2, V3)

# setwd("G:/R/10.16")
m <- read.csv("4.csv")
m4 <- t(m)
m4 <- as.data.frame(m4)
years = c(1975,1980,1985,1990,seq(from = 1995, to = 2015))
m5 <- cbind(m4, years)
m6 <- as.data.frame(m5)
library("Rcmdr")
attach(m6)
scatter3d(V1, V2, V3)




##2##############################################3
setwd("G:/R/10.17")
library(ggplot2)
data("midwest")
qplot(percwhite, percbelowpoverty, data = midwest)

#大小标度
qplot(percwhite, percbelowpoverty, data = midwest,
      size = poptotal/1e6)
ls <- geom_smooth(method = lm, size = 1)
qplot(percwhite, percbelowpoverty, data = midwest) + ls
qplot(percwhite, percbelowpoverty, data = midwest,
     size = poptotal/1e6) + ls
qplot(percwhite, percbelowpoverty, data = midwest,
      weight = poptotal/1e6, size = poptotal/1e6) + ls

#颜色标度     
qplot(percwhite, percbelowpoverty, colour = category, data = midwest,
      size = poptotal/1e6)

#形状标度
qplot(percwhite, percbelowpoverty, colour = category, data = midwest,
      size = poptotal/1e6, shape = state)








p <- qplot(cty, hwy,data = mpg, colour = displ)
p
p + scale_x_continuous("City mpg")
p + xlab("City mpg")
p + ylab("Highway mpg")
 


p <- qplot(sleep_total, sleep_cycle, data = msleep,
           colour = vore)
p
p + scale_colour_hue("What does\nit eat?",
                     breaks = c("herbi", "carni"),
                     labels = c("plants", "meat"))










qplot(years, V2/V1, data = m6, geom = c("point", "smooth"),
      span = 0.4)
qplot(years, V1, data = m6)
qplot(years, V1, data = m6, geom = "line")
qplot(V3, V1, data = m6, geom = c("point","path"), colour = years) 


p <- ggplot(m6, aes(years, V1))
p <- p + layer(geom = "point")


d <- ggplot(diamonds, aes(depth)) 
d + geom_histogram(aes(fill = cut), binwidth = 0.1, position = "fill")




#title(main = "M", ylab = "c") 
  #geom_point(aes(years, m6$V1))
  
title(main = "M", ylab = "c")               
              







## zuoye  ############################################
setwd("G:/R/jl1")

m2 <- read.csv("wine2.csv")
m3 <- m2[,c(15,16,17,18)]

f2<- lm(log(PRICE) ~ TIME, data = m3)
library(ggplot2)
qplot(TIME, log(PRICE), data = m3, geom = c("point", "smooth"),
      method = "lm")
#abline(f2)
summary(f2)

#t test
t <- (3 - 0.027759)/0.002266
t
rm(t)







#simple regression
setwd("G:/R/jl1")
m4 <- read.csv("wine3.csv")
m5 <- m4[,c(15,16,17,18)]
attach(m5)
f3<- lm(log(PRICE) ~ TIME, data = m5)
library(ggplot2)
qplot(TIME, log(PRICE), data = m5, geom = c("point", "smooth"),
      method = "lm")
summary(f3)
detach(m5)


#second regression
attach(m5)
f4<- lm(log(PRICE) ~ TIME + log(RAIN), data = m5)
#plot(TIME, log(PRICE))
library("Rcmdr")
attach(m5)
scatter3d(TIME,log(PRICE), log(RAIN))
summary(f4)
detach(m5)

#last regression
attach(m5)
f5<- lm(log(PRICE) ~ TIME + log(RAIN) + log(TEMP), data = m5)
summary(f5)

point <- data.frame(c(2, log(72.5), log(20.15)))
#point <- data.frame(2, log(72.5), log(20.15))
#point <- data.frame(TIME = 2, log(RAIN) = log(72.5), log(TEMP) = log(20.15))
predict(lm(f5),
        point, interval = "prediction", level = 0.95)



spreadLevelPlot(f5)

5.525608+6.169589
d3 <- log(m5$PRICE)
d4 <- mean(d3)
d5 <- d3-d4
d6 <- d5^2
a1 <- sum(d6)

d3 <- f5$fitted.values
d4 <- mean(d3)
d5 <- d3-d4
d6 <- d5^2
a2 <- sum(d6)

0.16471^2 + 0.2046519
sqrt(0.2317813)
5.43+1.96*0.4814
5.43-1.96*0.4814

attach(m5)
f6<- lm(log(PRICE) ~ I(TIME-2) + I(log(RAIN)-log(72.5)) + I(log(TEMP)-log(20.15)), data = m5)
summary(f6)
detach(m5)

#F test
anova(f3, f5)

-8.495372 + 0.034924*2 + -0.704211*log(72.5) +  5.618405*log(20.15)

0.16741
5.43-1.96*0.16471


#-8.495372 + 0.034924*1 + -0.704211*(19) +  5.618405*19.6






attach(m5)
f6<- lm(log(RAIN) ~ TIME, data = m5)
plot(TIME, log(RAIN))
abline(f6)
summary(f6)
detach(m5)


# ax reg
attach(m5)
f7<- lm(TIME ~ log(RAIN) + log(TEMP), data = m5)
summary(f7)
detach(m5)

attach(m5)
f8<- lm(log(RAIN) ~ TIME + log(TEMP), data = m5)
summary(f8)
detach(m5)

attach(m5)
f9<- lm(log(TEMP) ~ TIME + log(RAIN), data = m5)
summary(f9)
detach(m5)

option(digits = 2)

library("reshape", lib.loc="C:/Program Files/R/R-3.4.1/library")
md <- melt(mydata, id = c("id", "time"))
newdata <- cast(md, id+time ~ variable, mean)



## ch8 ###########################
setwd("G:/R/chap")
d <- read.table("LOANAPP.RAW")
e <- read.csv("LOANAPP.DES")
head(d)
e
str(d)
library(AER)
data("SwissLabor")
swiss_probit <- glm(participation ~ . + I(age^2),
                    data = SwissLabor, family = binomial(link = "probit"))
summary(swiss_probit)


d_g1 <- glm(V1 ~ V4 + V5 + V6 + V7 + V8,
            data = d)
summary(swiss_probit)







attach(d)
z1 <- subset(d, V5 == 1,
             select = c(1, 5))






## ch7 #######################################
setwd("G:/R/chap")
d <- read.table("GPA1.RAW")
t <- read.csv("GPA1.DES")
head(d)
t
str(d)
f <- lm(V10 ~ V19 + V11 + V12, data = d)
summary(f)
f2 <- lm(V10 ~ V19 + V11 + V12 + V29 + V28, data = d)
summary(f2)

anova(f, f2)




