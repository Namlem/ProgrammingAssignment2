## These functions will compute the inverse of a matrix and store
## it in a cache. If requested again, the data will be retrieved from
## the cache instead of being computed again

## takes a matrix as an argument, outputs a list of functions to set or get the
## matrix or its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # this function can be used to set a new matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
        
  get <- function(){x}
  setinv <- function(inverse){inv <<- inverse}
  getinv <- function(){inv}
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}

## uses the functions output by makeCacheMatrix to compute the inverse of 
## a matrix or retrieve the inverse from a cache

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  # check if inv has a value already; if so, return it
  if(!is.null(inv)){
    return(inv)
  }
  
  data <- x$get()
  # solve() computes the inverse of a matrix if it is passed only one matrix 
  inv <- solve(data)
  x$setinv(inv)
  inv
  
}
