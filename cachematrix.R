## Caching the Inverse of a Matrix
## Caches the inverse of a matrix (usually a costly computation)
##
## Usage
## testmat: a square matrix
## > matinv <- makeCacheMatrix(testmat)
## > cacheSolve(matinv)
##   computes and outputs inverse of matrix
## > cacheSolve(matinv)
##   outputs inverse of matrix from cache

## Creates functions to store the matrix and caches inverse of it

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ## get and set the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  ## get and set the inverse of matrix
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  ## return list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x' (makeCacheMatrix object)

cacheSolve <- function(x, ...) {
  ## Retrieve from cache
  m <- x$getinverse()
  
  ## check if the inverse already exists in the cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## get matrix and compute the inverse
  data <- x$get()
  m <- solve(data, ...)
  x$setinveres(m)
  m
}
