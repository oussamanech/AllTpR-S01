################################################################################
# BASICS OF DATA SCIENCE. Lecture course
# 2017-09-25. Regression models. Code samples.
#
# Dzimitry Markouski 
################################################################################

# 1. Linear regression. Basic example ------------------------------------------
# Install package with lots of interesting datasets
#install.packages("UsingR")
install.packages("UsingR")
library(UsingR)

data("galton")
View(galton)

# transform dataset to a "long" form
install.packages("reshape")
library(reshape)
data_long <- melt(galton)


g <- ggplot(data_long, aes(x = value, fill = variable)) +
  geom_histogram(colour = "black", binwidth = 1) +
  facet_grid(. ~ variable)
g


## Introductory data example
## Simple plot
library(dplyr)
ggplot(galton, aes(x=parent, y=child)) + geom_point()

## Advanced plot
# construct table for different combinations of parent-child heights
freqData <- as.data.frame(table(galton$child,galton$parent))
names(freqData) <- c("child", "parent", "freq")

# convert to numeric values
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))

# filter only meaningful combinations
g <- ggplot(filter(freqData, freq > 0), aes(x=parent, y=child))
g <- g + xlab("parent's height (in)") + ylab("child's height(in)")
g <- g + scale_size(range=c(2,10), guide = "none")

# plot grey circles slightly larger than data as base (achieve an outline effect)
g <- g + geom_point(colour = "grey50", aes(size=freq+5))

# plot the accurate data points
g <- g + geom_point(aes(colour=freq, size=freq))

# change the color gradient from default to lightblue -> white
g <- g + scale_color_gradient(low="lightblue", high = "white")
g


# 2. Regression through the origin ---------------------------------------------
## Same plot with manipulator
install.packages("manipulate")
library(manipulate)

myPlot <- function(data, beta, xmin, ymin){
  g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child)) +
    scale_size(range = c(2, 20), guide = "none" ) +
    coord_cartesian(xlim = c(xmin, 76), ylim = c(ymin, 75)) +
    geom_point(colour="grey50", aes(size = freq+5)) +
    geom_point(aes(colour=freq, size = freq/2)) +
    scale_colour_gradient(low = "lightblue", high="white") +
    geom_abline(intercept = 0, slope = beta, size = 3)
  rmse <- sqrt( mean( (galton$child - beta * galton$parent) ^2 ) )
  g <- g + ggtitle(paste("beta = ", beta, "rmse = ", round(rmse, 3)))
  g
}
manipulate(myPlot(data = freqData, beta, xmin, ymin),
           beta = slider(0.8, 1.2, step = 0.01),
           xmin = slider(0.0, 70, initial = 60, step = 2),
           ymin = slider(0.0, 70, initial = 60, step = 2))
# min rmse = 2.334, beta = 1.0

# quick modelling
model <- lm(galton$child ~ galton$parent - 1)
coef(model)

# add regression line to the plot
g + geom_abline(intercept = 0, slope = coef(model)[1])

# Mean squared error
summary(model)
sqrt(mean(model$residuals^2))

# 3 Regression through the center ----------------------------------------------
y <- galton$child - mean(galton$child)
x <- galton$parent - mean(galton$parent)
freqData <- as.data.frame(table(x, y))
names(freqData) <- c("child", "parent", "freq")
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))

# same plot with centered variables
myPlot <- function(beta){
  g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child)) +
    scale_size(range = c(2, 20), guide = "none" ) +
    geom_point(colour="grey50", aes(size = freq+5)) +
    geom_point(aes(colour=freq, size = freq)) +
    scale_colour_gradient(low = "lightblue", high="white") +
    geom_abline(intercept = 0, slope = beta, size = 3)
  rmse <- sqrt(mean( (y - beta * x) ^2 ))
  g <- g + ggtitle(paste("beta = ", beta, "rmse = ", round(rmse, 3)))
  g
}
manipulate(myPlot(beta), beta = slider(0.6, 1.2, step = 0.02))
# Result is that beta=0.64 gives the minimum rmse = 2.237

# Quick calculation can be done by the following
model <- lm( I(child - mean(child)) ~ I(parent - mean(parent)), data=galton)
summary(model)

model <- lm(galton$child ~ galton$parent)
summary(model)
coef(model)
# add regression line to the plot
g + geom_abline(intercept = coef(model)[1], slope = coef(model)[2], size = 2, 
                colour = grey(.5))


# 4. Total variation, R^2 ------------------------------------------------------
y <- galton$child
x <- galton$parent
model <- lm(y ~ x)

intercept <- coef(model)[1]
beta <- coef(model)[2]
y_fitted <- intercept + beta * x

# Total variation
total_variation <- sum((y - mean(y))^2)

# Residual variation
residual_variation <- sum((y - y_fitted)^2)

# Regression variation
regression_variation <- sum((y_fitted - mean(y))^2)

total_variation == residual_variation + regression_variation

round(total_variation, 3) == round(residual_variation + regression_variation, 3)

# R squared 
R2 <- regression_variation/total_variation

# R squared adjusted
k <- 1   # number of independent regressors
n <- 928 # number of points
R2_adj <- 1 - (((1-R2)*(n-1))/(n - k - 1))

summary(model)


# 5. Confidence intervals ------------------------------------------------------
# Diamonds price prediction
data(diamond)
g = ggplot(diamond, aes(x = carat, y = price))
g = g + xlab("Mass (carats)")
g = g + ylab("Price (SIN $)")
g = g + geom_point(size = 6, colour = "black", alpha=0.2)
g = g + geom_point(size = 5, colour = "blue", alpha=0.2)
g = g + geom_smooth(method = "lm", colour = "black")
g

# Basic Linear regression model
y <- diamond$price; x <- diamond$carat; n <- length(y)
fit <- lm(y ~ x)
coef(fit)
summary(fit)

# Residuals study
# unbiased RMSE (root mean squared error)
sigma <- sqrt(sum(resid(fit)^2) / (n-2))
# the same automatically (residual standard error)
summary(fit)$sigma

# Inference in Refression (p-values, t-values, Std.Errors for betas)
library(UsingR); data(diamond)
y <- diamond$price; x <- diamond$carat; n <- length(y)
beta1 <- fit$coefficients[2]
beta0 <- fit$coefficients[1]

# Gaussian regression error
e <- y - (beta0 + beta1 * x)

# unbiased estimate for variance
sigma <- sqrt(sum(e^2) / (n-2))
ssx <- sum((x - mean(x))^2)

#Standard errors of beta0 and beta1
seBeta0 <- sigma * (1/n + mean(x)^2 / ssx)^0.5
seBeta1 <- sigma / sqrt(ssx)

# testing for H0: beta0 = 0 and beta1 = 0
tBeta0 <- (beta0 - 0) / seBeta0
tBeta1 <- (beta1 - 0) / seBeta1

#p-values for Ha: beta0!=0 and beta1!=0 (two sided)
pBeta0 <- 2 * pt(abs(tBeta0), df = n - 2, lower.tail = FALSE)
pBeta1 <- 2 * pt(abs(tBeta1), df = n - 2, lower.tail = FALSE)

#store results into table
coefTable <- rbind(c(beta0, seBeta0, tBeta0, pBeta0),
                   c(beta1, seBeta1, tBeta1, pBeta1))
colnames(coefTable) <- c("Estimate", "Std. Error", "t-value", "P(>|t|)")
rownames(coefTable) <- c("(Intercept)", "x")
coefTable

# same coefs using r-function
fit <- lm(y ~ x); summary(fit)$coefficients

# confidence intervals for beta0 and beta1
sumCoef <- summary(fit)$coefficients
sumCoef[1] + c(-1,1) * qt(0.975, df = fit$df) * sumCoef[1,2]
(sumCoef[2] + c(-1,1) * qt(0.975, df = fit$df) * sumCoef[2,2])/10
# with 95% confidence we estimate that 0.1 carat increase in size
# results in a 355.6 to 388.6 increase in price


# 6. P R E D I C T I O N -------------------------------------------------------
y <- diamond$price; x<-diamond$carat; n<-length(y)
fit <- lm(y ~ x)

new_value <- 0.16   # Carat
prediction <- predict(fit, newdata = data.frame(x = new_value))

# Confidence intervals for predictions
predict(fit, newdata = data.frame(x = new_value), interval = "prediction")

# create a sequence of values that we want to predict at
newx = data.frame(x = seq(min(x), max(x), length = 100))

# calculate values for both intervals
p1 = data.frame(predict(fit, newdata = newx, interval = ("confidence")))
p2 = data.frame(predict(fit, newdata = newx, interval = ("prediction")))

# add columns for interval labels and newx values
p1$interval = "confidence"; p2$interval = "prediction"
p1$x = newx$x; p2$x = newx$x
dat = rbind(p1,p2)
names(dat)[1] = "y"

g = ggplot(dat, aes(x = x, y = y))
g = g + geom_ribbon(aes(ymin = lwr, ymax = upr, fill = interval), alpha = 0.2) +
  geom_line() +
  geom_point(data = data.frame(x = x, y = y), aes(x=x, y=y), size = 4)
g

# Revealing Non-linearity in data using residuals plot
x = runif(100,-3,3); y = x + sin(x) + rnorm(100,sd = 0.2);
g = ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) +
  geom_point(size = 7, color = "black", alpha = 0.4) +
  geom_point(size = 5, color = "red", alpha = 0.4)
g
g = g + geom_smooth(method = "lm", color = "black")
g

# Residuals vs X clearly shows nonlinearity
g = ggplot(data.frame(x=x, y = resid(lm(y ~ x))), aes(x=x, y=y)) +
  geom_point(size = 7, color = "black", alpha = 0.4) +
  geom_point(size = 5, color = "red", alpha = 0.4) +
  geom_hline(yintercept = 0, size = 2) + xlab("X") + ylab("Residual")
g

# Less obvious example of heteroskedasticity
x = runif(100,0,6); y = x + rnorm(100,mean = 0, sd = 0.001*x);
g = ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) +
  geom_point(size = 7, color = "black", alpha = 0.4) +
  geom_point(size = 5, color = "red", alpha = 0.4)
g = g + geom_smooth(method = "lm", color = "black")
g

# Residuals vs X shows trand toward greater variability
g = ggplot(data.frame(x=x, y = resid(lm(y ~ x))), aes(x=x, y=y)) +
  geom_point(size = 7, color = "black", alpha = 0.4) +
  geom_point(size = 5, color = "red", alpha = 0.4) +
  geom_hline(yintercept = 0, size = 2) + xlab("X") + ylab("Residual")
g

# Diamonds 
diamond$e <- resid(lm(price ~ carat, data = diamond))
g = ggplot(diamond, aes(x = carat, y = e)) +
  geom_point(size = 7, color = "black", alpha = 0.5) +
  geom_point(size = 5, color = "blue", alpha = 0.2) +
  geom_hline(yintercept = 0, size = 2) +
  xlab("Mass (carats)") + ylab("Residuals (SIN $)")
g

e = c(resid(lm(price ~ 1, data = diamond)),
      resid(lm(price ~ carat, data = diamond)),
      resid(lm(price ~ carat - 1, data = diamond)))
fit = factor(c(rep("Itc", nrow(diamond)),
               rep("Itc, slope", nrow(diamond)),
               rep("slope", nrow(diamond))))
ggplot(data.frame(e = e, fit = fit), aes(x = fit, y = e, fill = fit))+
  geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 20) +
  xlab("Fitting approach") + ylab("Residual price")

