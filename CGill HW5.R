#Question 1 part 1
library(forecast)
library(gains)
p5.df <- read.csv("/Users/Cassie Gill/OneDrive/SCMA 854/CGill HW5/SCMA 854 HW5/p5.csv")
View(p5.df)
p5.df <- p5.df[!is.na(p5.df$sales),]

#Question 1 part 2
set.seed(42)
training <- sample(p5.df$ID,300)
validation <- sample(setdiff(p5.df$ID, training), 206)

#Question 1 part 3
reg <- lm(sales~., data = p5.df[,-c(3,5)], subset=training, na.action=na.exclude)

par(mfcol = c(2, 2))
plot(reg)

#Question 1 part 4
pred_t <- predict(reg, na.action = na.pass)
pred_v <- predict(reg, newdata = p5.df[validation, -c(3,5)], na.action = na.pass)

accuracy(pred_t, p5.df[training,]$sales)
accuracy(pred_v, p5.df[validation,]$sales)

#Question 1 part 5
par(mfrow = c(2,2))

boxplot(residuals(reg), col = "gold", ylim=c(-5000, 5000), xlab = "Training Residuals")
resid_v <- p5.df[validation,3]- pred_v

boxplot(resid_v, col = "darkgreen" , ylim=c(-8000, 5000), xlab = "Validation Residuals")

hist(residuals(reg), xlim=c(-5000, 5000), ylab = "Training", main = "", xlab = "", col = "yellow")
hist(resid_v, xlim = c(-6000, 5000), ylab = "Validation", main = "", xlab = "", col = "green")

#Question 1 part 6
gain <- gains(p5.df[validation,]$sales[!is.na(pred_v)], pred_v[!is.na(pred_v)])
gain

options(scipen = 999)

sales <- p5.df[validation,]$sales[!is.na(p5.df[validation,]$sales)]

par(mfcol =c(2,1))
plot(c(0, gain$cume.pct.of.total*sum(sales))~c(0,gain$cume.obs),
     xlab = "Transaction Count", ylab = "Cumulative Sales", main= "Lift Chart", type="l")

lines(c(0, sum(sales)) ~ c(0, dim(p5.df[validation,])[1]), col="gray", lty=2)

barplot(gain$mean.resp/mean(sales), names.arg = gain$depth,
        xlab = "Percentile", ylab = "Mean Response", main = "Decile-Wise Lift Chart")

#Question 2 part 1
library(caret)
p7.df <- read.csv("/Users/Cassie Gill/OneDrive/SCMA 854/CGill HW5/SCMA 854 HW5/p7.csv")
View(p7.df)
str(p7.df)

p7.df$Actual <- as.factor(p7.df$Actual)
str(p7.df)

#Question 2 part 2 
table(ifelse(p7.df$Propensity>0.5, '1', '0'))
table(p7.df$Actual)

confusionMatrix(as.factor(ifelse(p7.df$Propensity>0.5, '1', '0')),
                p7.df$Actual)

confusionMatrix(as.factor(ifelse(p7.df$Propensity>0.25, "1", "0")),
                p7.df$Actual)

confusionMatrix(as.factor(ifelse(p7.df$Propensity>0.75, "1", "0")),
                p7.df$Actual)

#Question 2 part 3
p7.df <- read.csv("/Users/Cassie Gill/OneDrive/SCMA 854/CGill HW5/SCMA 854 HW5/p7.csv")
str(p7.df)

gain2 <- gains(p7.df$Actual, p7.df$Propensity)
barplot(gain2$mean.resp / mean(p7.df$Actual), names.arg = gain$depth, xlab = "Percentile", 
        ylab = "Mean Response", main = "Decile-wise lift chart")


