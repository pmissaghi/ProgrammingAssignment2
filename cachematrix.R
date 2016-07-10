## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a matrix, gets its inverse, and caches the inverse.
makeCacheMatrix <- function(x = matrix()) {
  i = NULL
  set = function(y) {
    x <<- y
    i <<- NULL
  }
  get = function() x
  seti = function(inverse) i <<- inverse 
  geti = function() i
  list(set=set, get=get, seti=seti, getinv=geti)
}


## Write a short comment describing this function
## Calculates the inverse of matrix from makeCacheMatrix(). checks cache to see if an inverse has already been calculated.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i = x$geti()
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  
  data = x$get()
  i = solve(data, ...)
  x$seti(i)
  i
}

