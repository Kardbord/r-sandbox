#!/usr/bin/env Rscript

.assert_numeric <- function(input) {
  if (!is.numeric(input)) {
    stop(
      sprintf(
        "Error: %s must be numeric, but it is actually %s",
        as.character(input), typeof(input)
      )
    )
  }
}

.split <- function(input) {
  .assert_numeric(input)
  if (length(input) <= 1) {
    return(list(l = input, r = c()))
  }
  mid <- floor(length(input) / 2)
  return(list(l = input[1:mid], r = input[(mid + 1):length(input)]))
}

# Merge two sorted lists
.merge <- function(left, right) {
  .assert_numeric(left)
  .assert_numeric(right)
  sorted <- c()

  while (length(left) > 0 && length(right) > 0) {
    if (left[1] < right[1]) {
      sorted <- c(sorted, left[1])
      left <- left[-1]
    } else {
      sorted <- c(sorted, right[1])
      right <- right[-1]
    }
  }

  sorted <- c(sorted, left, right)
  return(sorted)
}

# Sort a list
merge_sort <- function(input) {
  .assert_numeric(input)
  if (length(input) == 0 || length(input) == 1) {
    return(input)
  }
  split <- .split(input)
  return(.merge(merge_sort(split$l), merge_sort(split$r)))
}

sorted_list <- merge_sort(c(1, 4, 2, 5, 1, 22, 4))
print(sorted_list)
