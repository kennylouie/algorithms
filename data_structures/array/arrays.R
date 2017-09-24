# array data structure
# replicating array logic in R

# key characteristics:
# able to store element
# all elements in an array are the same type
# each location of an element in an array has a numerical index

# strengths:
# random access is fast

# construct an array
constructArray <- function() {
  a <- list()
  return(a)
}

# insert at start of array
insertStartOfArray <- function(array, value) {
  newArray <- list()
  newArray[[1]] <- value
  lengthArray <- length(array)
  if (lengthArray > 0) {
    for (ii in 1:lengthArray) {
      newArray[[ii+1]] <- array[[ii]]
    }
  }
  return(newArray)
}
# comments: time complexity O(1)

# insert at end of array
insertEndOfArray <- function(array, value) {
  newArray <- list()
  lengthArray <- length(array)
  ii <- 0
  if (lengthArray > 0) {
    for (ii in 1:lengthArray) {
      newArray[[ii]] <- array[[ii]]
    }
  }
  newArray[[ii+1]] <- value
  return(newArray)
}
# comments: time complexity O(1)

# insert into array at specified index
insertIntoArray <- function(array, value, index) {
  # standardizing the index
  lengthArray <- length(array)
  if (index < 2) {
    stop("Index cannot be at the beginning or less than 1. Considerusing insertStartOfArray()")
  }
  if (index >= lengthArray) {
    stop("Index cannot be greater than the current array length. Consider using insertEndOfArray()")
  }
  # construct new array
  newArray <- list()
  jj <- 1
  for (ii in 1:(lengthArray+1)) {
    if (ii == index) {
      next
    }
    newArray[[ii]] <- array[[jj]]
    jj <- jj + 1
  }
  newArray[[index]] <- value
  return(newArray)
}
# comments: time complexity O(N)

# delete at start of array
deleteStartOfArray <- function(array) {
  newArray <- list()
  lengthArray <- length(array)
  for (ii in 2:lengthArray) {
    newArray[[ii-1]] <- array[[ii]]
  }
  return(newArray)
}
# comments: time complexity O(1)

# delete at end of array
deleteEndOfArray <- function(array) {
  newArray <- list()
  lengthArray <- length(array)
  for (ii in 1:(lengthArray-1)) {
    newArray[[ii]] <- array[[ii]]
  }
  return(newArray)
}
# comments: time complexity O(1)

# delete at index of array
deleteWithinArray <- function(array, index) {
  # standardizing the index
  lengthArray <- length(array)
  if (index < 2) {
    stop("Index cannot be at the beginning or less than 1. Considerusing deleteStartOfArray()")
  }
  if (index >= lengthArray) {
    stop("Index cannot be greater than the current array length. Consider using deleteEndOfArray()")
  }
  # construct new array
  newArray <- list()
  jj <- 1
  for (ii in 1:(lengthArray)) {
    if (ii == index) {
      next
    }
    newArray[[jj]] <- array[[ii]]
    jj <- jj + 1
  }
  return(newArray)
}
# comments: time complexity O(N)

# accessing array
accessValueInArray <- function(array, index) {
  value <- array[[index]]
  return(value)
}
# comments: time complexity O(N)

# search for element in array
searchArray <- function(array, value) {
  lengthArray <- length(array)
  output <- "not found"
  for (ii in 1:lengthArray) {
    if (array[[ii]] == value) {
      output <- ii
    }
  }
  return(output)
}
# comments: time complexity O(N)

# testing
(test <- constructArray())
(test <- insertStartOfArray(test, 1))
(test <- insertStartOfArray(test, 1))
(test <- insertEndOfArray(test, 2))
(test <- insertEndOfArray(test, 2))
(test <- insertIntoArray(test, 3, 0))
(test <- insertIntoArray(test, 3, 5))
(test <- insertIntoArray(test, 3, 3))
(test <- insertIntoArray(test, 3, 4))
(test <- deleteStartOfArray(test))
(test <- deleteEndOfArray(test))
(test <- deleteWithinArray(test, 3))
(test <- deleteWithinArray(test, 2))
(test <- deleteWithinArray(test, 1))
(test <- deleteWithinArray(test, 2))
accessArray(test, 1)
accessArray(test, 2)
accessArray(test, 1)
searchArray(test, 2)
searchArray(test, 1)