# test data (True gaussian mixture)
library(mvtnorm)
set.seed(999)
nclust <- 4
d <- 2
mu <- list(c(.75, .5), c(.5, .75), c(.25, .5), c(.75, .3))
S <- list(matrix(.1^2 * c(1, .5, .5, 1), 2, 2), .05^2 * diag(2),
          matrix(.1^2 * c(1, -.5, -.5, 1), 2, 2), .075^2 * diag(2))
n <- 1000
k <- sample(1:4, n, TRUE)
y <- matrix(0, n, d)
for (i in 1:nclust) {
  idx <- (k == i)
  print(mu[[i]])
  y[idx, ] <- rmvnorm(sum(idx), mu[[i]], S[[i]])
}
summary(y)
plot(y, xlim = c(0, 1), ylim = c(0, 1), bg = k, pch = 21,
     main = "Simulated normal mixture data")

system.time({
  res <- dp_normal_mix(
    y[ , ], 
    N = 100,
    alpha = 4, 
    lambda = runif(2), 
    kappa = 1, 
    nu = 2,
    Omega =  0.13 ^ 2 * diag(2))  
})


points(t(res$meanj), bg = "yellow", pch = 21)

resol <- 100
mesh <- expand.grid(x = seq(0, 1, length.out = resol), y = seq(0, 1, length.out = resol))
mesh$z <- 0

m <- sum(res$nj[res$nj > 1])
idx <- which(res$nj > 1)
for (i in idx) {
  # if (res$nj[i] > 1) {
  #   covmat <-  res$Sj[ , ,i] / (res$nj[i] - 2) 
  # } else {
  #   covmat <- 1 * 0.15 ^ 2 * diag(2)
  # }
  mesh$z <- mesh$z + (res$nj[i] / res$m) * dmvnorm(mesh[ ,c("x", "y")], res$meanj[ , i, drop = TRUE], res$Sj[ , ,i] / (res$nj[i] - 2))
}
z <- matrix(mesh$z, resol, resol)
contour(z, add = TRUE)
# 
library(plotly)
plot_ly(z = ~t(z)) %>% add_heatmap()
# 
plot_ly(z = ~t(z)) %>% add_surface()