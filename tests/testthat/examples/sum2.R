## Force the core arrays to be in the initial conditions in order to
## make them slices of the state, rather than proper vectors.
y0[, , ] <- user()
dim(y0) <- user()
update(y[, , ]) <- y[i, j, k] # trivial model!
initial(y[, , ]) <- y0[i, j, k]
dim(y) <- c(dim(y0, 1), dim(y0, 2), dim(y0, 3))

## These collapse one dimension
update(m12[, ]) <- sum(y[i, j, ])
update(m13[, ]) <- sum(y[i, , j])
update(m23[, ]) <- sum(y[, i, j])

initial(m12[, ]) <- sum(y0[i, j, ])
initial(m13[, ]) <- sum(y0[i, , j])
initial(m23[, ]) <- sum(y0[, i, j])

dim(m12) <- c(dim(y, 1), dim(y, 2))
dim(m13) <- c(dim(y, 1), dim(y, 3))
dim(m23) <- c(dim(y, 2), dim(y, 3))

## These collapse two dimensions
update(v1[]) <- sum(y[i, , ])
update(v2[]) <- sum(y[, i, ])
update(v3[]) <- sum(y[, , i])

initial(v1[]) <- sum(y0[i, , ])
initial(v2[]) <- sum(y0[, i, ])
initial(v3[]) <- sum(y0[, , i])

dim(v1) <- dim(y, 1)
dim(v2) <- dim(y, 2)
dim(v3) <- dim(y, 3)

update(mm12[, ]) <- sum(y[i, j, 2:4])
update(mm13[, ]) <- sum(y[i, 2:4, j])
update(mm23[, ]) <- sum(y[2:4, i, j])

initial(mm12[, ]) <- sum(y0[i, j, 2:4])
initial(mm13[, ]) <- sum(y0[i, 2:4, j])
initial(mm23[, ]) <- sum(y0[2:4, i, j])

dim(mm12) <- c(dim(y, 1), dim(y, 2))
dim(mm13) <- c(dim(y, 1), dim(y, 3))
dim(mm23) <- c(dim(y, 2), dim(y, 3))

update(vv1[]) <- sum(y[i, 2:4, 2:4])
update(vv2[]) <- sum(y[2:4, i, 2:4])
update(vv3[]) <- sum(y[2:4, 2:4, i])

initial(vv1[]) <- sum(y0[i, 2:4, 2:4])
initial(vv2[]) <- sum(y0[2:4, i, 2:4])
initial(vv3[]) <- sum(y0[2:4, 2:4, i])

dim(vv1) <- dim(y, 1)
dim(vv2) <- dim(y, 2)
dim(vv3) <- dim(y, 3)

update(tot1) <- sum(y)
update(tot2) <- sum(y[, , ])

initial(tot1) <- sum(y0)
initial(tot2) <- sum(y0[, , ])
