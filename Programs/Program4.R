a <- array(
  1:12,
  dim = c(2, 3, 2),
  dimnames = list(
    Row = c("R1", "R2"),
    Column = c("C1", "C2", "C3"),
    Table = c("T1", "T2")
  )
)
print(a)

cat("Selected element:", a["R2", "C3", "T2"], "\n")

