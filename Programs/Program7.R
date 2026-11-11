n <- 12
factors <- which(n %% (1:n) == 0)
cat("Factors of", n, "are:\n")
print(factors)

set.seed(1)
random_numbers <- sample(-50:50, 10)
cat("Random integers:\n")
print(random_numbers)

cat("FizzBuzz sequence:\n")

for (i in 1:100) {
  if (i %% 15 == 0) {
    print("FizzBuzz")
  } else if (i %% 3 == 0) {
    print("Fizz")
  } else if (i %% 5 == 0) {
    print("Buzz")
  } else {
    print(i)
  }
}
