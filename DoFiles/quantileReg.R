library(quantreg)
data(mtcars)

multi_rqfit <- rq(mpg ~ wt, data = mtcars, tau = seq(0, 1, by = 0.1))
multi_rqfit

# plotting different quantiles
colors <- c("#ffe6e6", "#ffcccc", "#ff9999", "#ff6666", "#ff3333",
            "#ff0000", "#cc0000", "#b30000", "#800000", "#4d0000", "#000000")
plot(mpg ~ wt, data = mtcars, pch = 16, main = "mpg ~ wt")
for (j in 1:ncol(multi_rqfit$coefficients)) {
  abline(coef(multi_rqfit)[, j], col = colors[j])
}
