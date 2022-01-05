## Put comments here that give an overall description of what your
## functions do
##this functions allows to create a matrix and cache it inverse.
##this inverse can be retrieved without recomputing it if the matrix wasn't modified.

## Write a short comment describing this function
##This function return a list of functions that allow to interact with the matrix 
makeCacheMatrix <- function(x = matrix()) {
  inv <-NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(y) inv <<- y
  
  getinv <- function() inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
##this function gets the inverse from the matrix or compute it
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  
  if(!is.null(inv))
    return(inv)
  
  y <- x$get()
  inv <- solve(y)
  x$setinv(inv)
  
  inv
  
}
