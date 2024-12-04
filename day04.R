library(stringr)

wordLines <- readLines("day04.txt")
numCols <- length(wordLines)
lets <- matrix(data = unlist(str_split(wordLines, "")), ncol = numCols)
numRows <- dim(lets)[1]

xmas <- c("X", "M", "A", "S")
lXmas <- length(xmas)

sum(
  sapply(seq(numCols), function(nCol) {
    sapply(seq(numRows), function(nRow) {
      currentLetter <- lets[nRow, nCol]
      if (!currentLetter == xmas[1]) {
        return(0)
      }
      numXmas <- 0
      for (dx in -1:1) {
        for (dy in -1:1) {
          if (nRow + (lXmas - 1) * dy < 1 |
              nRow + (lXmas - 1) * dy > numRows |
              nCol + (lXmas - 1) * dx < 1 |
              nCol + (lXmas - 1) * dx > numCols) {
            next
          }
          maybeXmas <- sapply(seq(lXmas - 1), function(i) {
            lets[nRow + i * dy, nCol + i * dx]
          })
          numXmas <- numXmas + (all(maybeXmas == xmas[-1]))
        }
      }
      return(numXmas)
    })
  })
)

sum(
  sapply(2:(numCols - 1), function(nCol) {
    sapply(2:(numRows - 1), function(nRow) {
      currentLetter <- lets[nRow, nCol]
      if (!currentLetter == "A") {
        return(FALSE)
      }
      diag1 <- c(lets[nRow - 1, nCol - 1],
                 lets[nRow + 1, nCol + 1])
      if (!all(c("M", "S") %in% diag1)) {
        return(FALSE)
      }
      diag2 <- c(lets[nRow - 1, nCol + 1],
                 lets[nRow + 1, nCol - 1])
      return(all(c("M", "S") %in% diag2))
    })
  })
)
