initial(x[, ]) <- x0[i, j]
update(x[, ]) <- x[i, j] + r[i, j]
x0[, ] <- user()
r[, ] <- user()
dim(x0) <- user()
dim(x) <- c(dim(x0, 1), dim(x0, 2))
dim(r) <- c(dim(x0, 1), dim(x0, 2))
