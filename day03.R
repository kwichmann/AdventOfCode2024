library(stringr)

code <- readLines("day03.txt")

trueMults <- str_match_all(code, "mul\\((\\d+)\\,(\\d+)\\)")
sum(
  sapply(trueMults, function(mulList) {
    sum(as.numeric(mulList[,2]) * as.numeric(mulList[,3]))
  })
)
  
ops <- str_match_all(code, "mul\\((\\d+)\\,(\\d+)\\)|do\\(\\)|don't\\(\\)")
ops <- unlist(lapply(ops, t))

pc <- 1
accumulator <- 0
doSum <- TRUE

while (pc <= length(ops)) {
  currentOp <- ops[pc]
  if (is.na(currentOp)) {
    pc <- pc + 1
    next
  }
  if (currentOp == "do()") {
    doSum <- TRUE
    pc <- pc + 1
    next
  }
  if (currentOp == "don't()") {
    doSum <- FALSE
    pc <- pc + 1
    next
  }
  if (doSum) {
    accumulator <- accumulator + as.numeric(ops[pc + 1]) * as.numeric(ops[pc + 2])
  }
  pc <- pc + 3
}

print(accumulator)
