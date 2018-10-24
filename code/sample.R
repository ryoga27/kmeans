rm(list = ls(all = TRUE))
source("model.R")
data(iris)
x = as.matrix(iris[, 1:4])
fit_k_means = kmeans(x, K = 3)
fit_z = fit_k_means$z
true_z = as.matrix(iris[, 5])
table(true_z, fit_z)
