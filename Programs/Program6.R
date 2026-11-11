double_value <- function(x) {
  x * 2
}

my_list <- list(
  numbers = c(2, 4, 6),
  marks = matrix(c(70, 80, 90, 95), nrow = 2),
  double = double_value
)

print(my_list)

print(my_list$numbers)

print(my_list$double(5))

