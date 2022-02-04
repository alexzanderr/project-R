

f1_caciula <- function(x1, x2) {
    result <- exp((-1 / 2) * (((x1 ^ 2) / 4) + (x2 ^ 2)))
    return (result)
}


f2_caciula <- function(y1, y2) {
    # exp_ <- exp((-1 / 2) * (((y1 ^ 2) / 4) + (y2 ^ 2)))
    exp_ <- f1_caciula(y1, y2)
    cos_sin <- (cos(y1) ^ 2) + (0.5 * (sin(3 * y2) ^ 2) * (cos(y1) ^ 4))

    return (exp_ * cos_sin)
}

# result <- f1_caciula(1, 1)
# print(result)

# result <- f2_caciula(1, 1)
# print(result)


# png(file = "plot.png")
# par(mfrow = c(1, 2))

x1 <- seq(-15, 15, 0.01)
x2 <- seq(-15, 15, 0.01)
x1_x2_data <- data.frame(x=x1, y=x2)

# f = f caciula * c
# fie c = 1
f1 <- f1_caciula
f2 <- f2_caciula


g <- function(x1, x2) {
    result <- exp((-x1 / 2) - x2)

    return (result)
}

raport_f1 <- function(x1, x2) {
    return (f1(x1, x2) / g(x1, x2))
}

raport_f2 <- function(x1, x2) {
    return (f2(x1, x2) / g(x1, x2))
}

# pentru f(2, -1) = e pe R^2
M1 <- raport_f1(2, 1)
# M2 <- raport_f2(2, -1)


raport_M1 <- function(x1, x2) {
    return (f1(x1, x2) / (M1 * g(x1, x2)))
}

print(M1)

# esantion de lungime 100000
x1_accepted <- c()
x2_accepted <- c()
n <- 1000
total_repeats <- 0
for (i in 1:n) {
    # acceptance_prob
    repeat {
        u1 <- runif(1)
        u2 <- runif(1)
        # x1 <- -(1/2) * log(rexp(1))
        # x2 <- log(rexp(1))

        x1 <- 1 + sqrt(-log(u1))
        x2 <- 1 + sqrt(-5.4 * log(u2))

        total_repeats <- total_repeats + 1

        if((u1 * u2) < raport_M1(x1, x2)) {
            break
        }
    }
    x1_accepted <- append(x1_accepted, x1)
    x2_accepted <- append(x2_accepted, x2)
}
acceptance_prob <- n / total_repeats
print(acceptance_prob)

# plot((x1, x2), f2_caciula(x1, x2), type = "l", col = "magenta") # trasam graficul functiei h

# dev.off()