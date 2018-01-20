# Arturo Perez
# Professor Doug Hardin
# Math 3890
# 20 September 2017

library(ggplot2)
library('ProjectTemplate')
library("reshape")
load.project()

# p 1
# Raw data
simulation.xs <- c(1959, 1960, 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969)
simulation.ys <- c(4835, 4970, 5085, 5160, 5310, 5260, 5235, 5255, 5235, 5210, 5175)
simulation.df <- data.frame(pop = simulation.ys, year = simulation.xs)

# Rescale years
simulation.df$year = simulation.df$year - 1964

# Generate regression, construct confidence intervals
fit <- lm(pop ~ year + I(year^2) + I(year^3), data=simulation.df)
xs = seq(-5, 5, 0.1)
fit.confidence = predict(fit, data.frame(year=xs), interval="confidence", level=0.95)


# Create data frame containing variables of interest
df = as.data.frame(fit.confidence)
df$year <- xs
df = melt(df, id.vars="year")

p <- ggplot() + geom_line(aes(x=year, y=value, colour=variable), df) + 
  geom_point(aes(x=year, y=pop), simulation.df)
p <- p + scale_x_continuous('Year') + scale_y_continuous('Population')
p <- p + ggtitle("Cubic regression with confidence intervals")
p <- p + scale_color_brewer(name="Legend",
                            labels=c("Fit", 
                                     "95% Lower Bound", 
                                     "95% Upper Bound"), 
                            palette="Set1")
ggsave(file.path('/Users/Arturo1/Desktop', 'exercise_3_2.pdf'))

# computatational
x <- vector(length = 10)
for (i in 1:length(x)) {
  x[i] <- (i-.5)/5
}
X <-  matrix(ncol = 4, nrow = 10)
X[,1] <- 1
for (j in 2:4) {
  X[,j] <- x^(j-1)
}

X<-as.data.frame(X)
X_T <- t(X)

X<-as.matrix(X)
X_T<-as.matrix(X_T)

beta <- c(1,6,-8,3)
sigma_sq <- 2

x0_T <- matrix(ncol=4, nrow=1, data=1)
x0 <- matrix(ncol=1,nrow=4, data=1)


CI_1 <- list(x0_T%*%beta - (1.96*sqrt(sigma_sq)*sqrt(x0_T %*% (solve(X_T %*% X)) %*% x0)), 
             x0_T%*%beta + (1.96*sqrt(sigma_sq)*sqrt(x0_T %*% (solve(X_T %*% X)) %*% x0)))


