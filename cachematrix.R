## Put comments here that give an overall description of what your
## functions do

## This function creates a list populated with the functions for [1] setting the valuesof the matrix,
## [2] getting the value of the matrix, [3] setting the value of the matrix's inverse and [4] getting
## the value of the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL ## Initially set the m argument to NULL
    set <- function(y) { ## Assign the set part of the list the function below
      x <<- y
      m <<- NULL
    }
    get <- function() {x} ## a function that just returns x
    setinv <- function(solve) {m <<- solve} ## a function assigns the function solve to function m
    getinv <- function() {m} ## a function that returns m
    
    list(set = set, get = get, setinv = setinv, getinv = getinv) ## create the list of the functions above
  
}

## This function will take the list output from the makeCacheMatrix function and compute the inverse matrix
## of x.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv() 
  if(!is.null(m)) { ## Check if this is the first time that the matrix has been computed
    message("getting cached data")
    return(m) ## Exit the function and return the value of m
  }
  data <- x$get() ## Retrieve the matrix to be inversed
  m <- solve(data) ## Run the solve function to compute the inverse
  x$setinv(m) ## set the value of the inverse in cache
  m ## return the value of m
}

