height <- factor(c("short", "tall", "medium", "tall"))
print(height)
print(levels(height))

set.seed(10)
letters_factor <- factor(sample(LETTERS, 5))

print(letters_factor)
print(levels(letters_factor))

