x <- runif(50, -2, 2)
y <- x + rnorm(50)

# Create the model object
mod <- lm(y ~ x)

# Create prediction values and confidence limits
# using a new dataframe of x values, noting the
# colnames need to match your model term names.
newData <- data.frame(x = seq(min(x), max(x), by = (max(x) - min(x)) / 49))
pred.lim <- predict(mod, newdata = newData, interval = "prediction")
conf.lim <- predict(mod, newdata = newData, interval = "confidence")

# Create the plot. Do not plot the data points
# and axes to allow us to define them our way
plot(x, y, xlab = "x vals", ylab = "y vals", type = "n", 
    main = "Linear Regression Plot", ylim = range(y, pred.lim, na.rm = TRUE))

points(x, y, pch = 21, bg = "yellow")

# Draw the fitted regression line and the
# prediction and confidence intervals
matlines(newData$x, pred.lim, lty = c(1, 4, 4), lwd = 2, col = c("black", "red", "red"))
matlines(newData$x, conf.lim, lty = c(1, 3, 3), lwd = 2, col = c("black", "green4", "green4"))

# Draw the legend
legend(-2, max(pred.lim, na.rm = TRUE),
legend = c("Fitted Line", "Confidence Bands", "Prediction Bands"),
  lty = c(1, 3, 4), lwd = 2, col = c("black", "green4", "red"),
  horiz = FALSE, cex = 0.9, bg = "gray95")
