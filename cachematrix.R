## Cache the matrix
## This function creates a special "matrix" object that can cache its inverse.
## This function will first received the matrix value through argument parameters x, then caches it to another cached matrix.

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
## then cacheSolve should retrieve the inverse from the cache and print out the "getting cached data" message.
## then output the inversed matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mInverse <- x$getInverse()
  ##if the same matrix has been calculated, the cached data will be return
  if(!is.null(mInverse)) {
    message("getting cached data")
    return(mInverse)
  }
  ##if the matrix has been altered or it is a new matrix, inverse calculation will be perform using solve function
  data <- x$get()
  mInverse <- solve(data, ...)
  x$setInverse(mInverse)
  mInverse
}

