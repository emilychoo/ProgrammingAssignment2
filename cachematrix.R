## Cache the matrix
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  cachedmInverse <- NULL
  set <- function(y) {
    x <<- y
    cachedmInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) cachedmInverse <<- inverse
  getInverse <- function() cachedmInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mInverse <- x$getInverse()
  if(!is.null(mInverse)) {
    message("getting cached data")
    return(mInverse)
  }
  data <- x$get()
  mInverse <- solve(data, ...)
  x$setInverse(mInverse)
  mInverse
}

##Testing Scripts:

##m <- matrix(c(-1, -2, 1, 1), 2,2)
##x <- makeCacheMatrix(m)
##x$get()
##[,1] [,2]
##[1,]   -1    1
##[2,]   -2    1

##inv <- cacheSolve(x)
##inv
##[,1] [,2]
##[1,]    1   -1
##[2,]    2   -1

##inv <- cacheSolve(x)
##getting cached data
##> inv
##[,1] [,2]
##[1,]    1   -1
##[2,]    2   -1
