library(stringr)

reports <- readLines("day02.txt")
levels <- lapply(str_split(string = reports, pattern = " "), as.numeric)

isSafe <- function(lvls) {
  diffs <- diff(lvls)
  return(
    length(unique(sign(diffs))) == 1 &
      all(abs(diffs) >= 1 & abs(diffs) <= 3)
  )
}

sum(sapply(levels, isSafe))

sum(sapply(levels, function(lvl) {
  if (isSafe(lvl)) return(TRUE)
  for (i in seq(length(lvl))) {
    if (isSafe(lvl[-i])) return(TRUE)
  }
  return(FALSE)
}))
