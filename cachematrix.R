## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#1. set and get the value of matrix
#2. set and get the value of inverse matrix

makeCacheMatrix <- function(x = matrix()) {

  inv = NULL
  set = function(y)
  {
    x <<- y
    inv <<- NULL
  }
  
  get = function() x
  setinv = function(inverse) inv <<- inverse
  getinv = function() inv
  list(set = set, get=get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
#this function will return the inverse of matrix
#if inverse of matrix is already computed then it will display it
#otherwise it will compute it and sets the value of inverse via setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv = x$getinv()
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  
  data = x$get()
  inv = solve(data,...)
  
  x$setinv(inv)
  return(inv)
}
