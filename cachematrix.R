## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly. 

## The makeCacheMatrix function creates a special "matrix" object that can 
## cache its inverse, which is a matrix containing a function to:
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse of the matrix
## 4.  get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The following function returns the inverse of the matrix created by makeCacheMatrix 
## above function. It first checks to see if the inverse of the matrix has 
## already been computed. If so, it `get`s the inverse of the matrix from the
## cache and skips the computation. Otherwise, it computes the inverse of
## the matrix and sets the value of the inverse in the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
