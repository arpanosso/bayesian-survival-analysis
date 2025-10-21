library("plotrix")
library("rstan")
library("shinystan")

# Simulating data
set.seed(2024)
# Sample size
n <- 1000
n_pred <- 20
# Design matrix
X <- model.matrix(~ rnorm(n))
X_pred <- model.matrix(~ rnorm(n_pred))
# Parameters
beta <- c(5, 3)
sigma <- 2
# Error term
error <- rnorm(n, 0, sigma)
error_pred <- rnorm(n_pred, 0, sigma)
# Response variable
y <- as.vector(X %*% beta + error)
y_pred <- as.vector(X_pred %*% beta + error_pred)

# Scatter plot
data <- data.frame(y = y, x = X[,2])
plot(data$x, data$y, pch=19)
# Linear regression
summary(fit <- lm(y ~ x, data = data))
abline(fit, col = "red", lwd = 2)
# Prediction
fit_pred <- predict(fit, newdata = data.frame(x=X_pred[,2]), interval = "prediction")
plotCI(x = 1:n_pred, y = fit_pred[,1], li = fit_pred[,2], ui = fit_pred[,3], 
       pch = 19, xlab = "Index", ylab = "Prediction", col = "red")
points(1:n_pred, y_pred, pch = 19, col = "green4")


# Linear regression with Stan
m0 <- stan(file   = "LM.stan", 
           data   = list(n = n, n_pred = n_pred, Nbetas = ncol(X), 
                         y = y, X = X, X_pred = X_pred),
           warmup = 200,                 
           iter   = 500,
           chains = 3,
           seed   = 0,
           cores  = getOption("mc.cores",3)) 

print(m0)


# Diagnostics
launch_shinystan(m0)

pars <- c("beta", "sigma")
plot(m0, plotfun = "trace", pars = pars, inc_warmup = TRUE)
plot(m0, plotfun = "hist", pars = pars)

post0 <- extract(m0, pars)
beta_mean <- apply(post0$beta, 2, mean)

# Regression line
plot(data$x, data$y, pch = 19)
abline(fit, col = "red", lwd = 2)
abline(coef = beta_mean, col = "blue", lwd = 2)

# Prediction
pred0 <- extract(m0, "y_pred")
pred_mean <- apply(pred0$y_pred, 2, mean)
pred_lwd <- apply(pred0$y_pred, 2, quantile, probs = 0.025)
pred_upr <- apply(pred0$y_pred, 2, quantile, probs = 0.975)

plotCI(x = 1:n_pred - 0.1, y = fit_pred[,1], li = fit_pred[,2], ui = fit_pred[,3], 
       pch = 19, xlab = "Index", ylab = "Prediction", col="red")
points(1:n_pred, y_pred, pch=19, col = "green4")
plotCI(x = 1:n_pred + 0.1, y = pred_mean, li = pred_lwd, ui = pred_upr, 
       pch = 19, add = TRUE, col = "blue")
