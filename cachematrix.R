## The following functions take a given matrix q and, through
## makeCacheMatrix, convert it into a special form capable of caching its
## inverse and, through cacheSolve, solves for the inverse or returns
## the previously cached value. cacheSolve() must be given a matrix in
## the special form (ie. for q<-c(1:16), cacheSolve(q) will not work but
## cacheSolve(makeCacheMatrix(q)) will)

## Makes a special matrix that can cache it's mean for ease of work
## EX: qcache <-makeCacheMatrix(q) , qcache is now a matrix with the
## ability to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computes the inverse of the special matrix return from the function above or
## retrieves the cached value if it exists.
## EX: cacheSolve(qcache) or cacheSolve(makeCacheMatrix(q)) will return the inverse
## of the orginal matrix q (or the previously cached value)

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
