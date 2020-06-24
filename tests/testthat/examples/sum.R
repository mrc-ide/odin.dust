m[, ] <- user()
dim(m) <- user()

update(v1[]) <- sum(m[i, ])
dim(v1) <- dim(m, 1)
update(v2[]) <- sum(m[, i])
dim(v2) <- dim(m, 2)

update(v3[]) <- sum(m[i, 2:4])
dim(v3) <- length(v1)
update(v4[]) <- sum(m[2:4, i])
dim(v4) <- length(v2)

update(tot1) <- sum(m)
update(tot2) <- sum(m[, ])

initial(v1[]) <- 0
initial(v2[]) <- 0
initial(v3[]) <- 0
initial(v4[]) <- 0
initial(tot1) <- 0
initial(tot2) <- 0
