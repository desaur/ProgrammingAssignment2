## Put comments here that give an overall description of what your
## functions do

## Function makeCacheMatrix to create a special "matrix" object that has its inverse cached

## Function cacheSolve to return the inverse of the matrix if it is not already calculated in the special "matrix" object

## Write a short comment describing this function

## Function makeCacheMatrix() creates the following list of matrix operations:
## 1. set the value of the underlying matrix
## 2. get the value of the matrix
## 3. set the value of the inverse (function) of the matrix
## 4. get the value of the inverse (function) of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(M) {
    x <<- M
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Write a short comment describing this function

## Function cacheSolve() checks of inverse of underlying matrix in 
## the special matrix is already calulated. If not, it calculates the 
## inverse using solve() and sets the value using "setinv" function of 
## the special object. Otherwise, it skips the calculations and returns
## the inverse by calling "getinv" function of the special object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("Getting cached inverse")
    return(inv)
  }
  M <- x$get()
  inv <- solve(M, ...)
  x$setinv(inv)
  inv
}
