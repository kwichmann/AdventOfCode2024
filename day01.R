day01 <- read.table("day01.txt", quote = "\"", comment.char = "")

rw1 <- day01[,1]
rw2 <- day01[,2]

sum(abs(sort(rw1) - sort(rw2)))

sapply(rw1, function(x) {
  x * sum(rw2 == x)
}) %>%
  sum()
