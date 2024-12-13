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

merge_sort <- function(input) {
  .assert_numeric(input)
  if (length(input) == 0 || length(input) == 1) {
    return(input)
  }
  split <- .split(input)
  return(.merge(merge_sort(split$l), merge_sort(split$r)))
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)

  if (length(args) == 0) {
    stop("Please provide a list of numbers to sort.")
  }

  inputs <- as.numeric(args)

  if (anyNA(inputs)) {
    stop("Please ensure all inputs are numeric values.")
  }

  sorted_list <- merge_sort(inputs)
  print(sorted_list)
}
