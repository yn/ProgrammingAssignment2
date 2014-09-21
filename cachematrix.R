## Together, these functions create a special "matrix" object that can cache its inverse.

## The function makeCacheMatrix implements the storage of special "matrix" object.
## It returns a list of closures that capture the variables x and i,
## where x is the matrix being stored, and i is the cached inverse.
## The four closures returned perform the following operations:
## 1. set the matrix. This clears the cached inverse
## 2. get the matrix
## 3. set the cached inverse
## 4. get the cached inverse

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

## The function cacheSolve returns the inverse of a matrix.
## It either returns it from the cache if it's 
## been called previously, or calculates a new one
## and caches it.

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
