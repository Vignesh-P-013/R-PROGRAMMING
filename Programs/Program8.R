set.seed(123)

x <- rnorm(20, mean = 0, sd = 1)

rounded_x <- round(x)


print(rounded_x)


print(table(rounded_x))