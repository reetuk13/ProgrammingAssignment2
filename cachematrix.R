## The below functions create a cache of the matrix and returns its value from
## cache if it has already been calculated

##  makeCacheMatrix: This function creates a special "matrix" object that can 
##  cache its inverse.

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


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not 
## changed), then the cachesolve should retrieve the inverse from the 
## cache. Computing the inverse of a square matrix can be done 
## with the solve function in R. For example, if X is a square 
## invertible matrix, then solve(X) returns its inverse.

cacheSolve <- function(x, ...) {
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
