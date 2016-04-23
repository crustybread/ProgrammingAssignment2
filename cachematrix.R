## Put comments here that give an overall description of what your
## functions do

## As described in the assignment, these functions read in a square matrix (assumed to be invertible) and cache its corresponding inverse matrix (if not already cached) or retrieve the existing cached inverse matrix without having to recompute it. 

## Write a short comment describing this function

## The makeCacheMatrix function takes in a square matrix (assumed to be invertible) as its argument. It has four functions, all of which are passed into the cacheSolve function for implementation. I'll describe those in cacheSolve below. Whenever this function is run it clears any existing value of i and lists the functions it returns.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}

## Write a short comment describing this function

## The cacheSolve function takes the square matrix and makeCacheMatrix's four functions as arguments. getinv retrieves the cached i if it exists. It prints the message "getting cached data" and returns i.  If i has no value, get is invoked to pass the square matrix to the variable data.  Once data is processed by the built-in function solve, setinv caches the inverted matrix to the parent environment on the variable i. If subsequent runs of the function are for the same matrix, getinv will pick it up without having to recompute. 


cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'

  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
