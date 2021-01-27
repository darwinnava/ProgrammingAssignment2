##Functions for caching. A way to store objects in memory!
## It is going to accelerate subsequent access to the same object

##Create an R object that stores a matrix and its inverse
##Initialize objects
makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  ##Define funcions for objects of the type makeCacheMatri
  get <- function() x
  setinv <- function(inv) im <<- inv
  getinv <- function() im
  ##Create a new object by returning a list
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve is required to populate and/or retrieve
## the inverse from an object of type makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  im <- x$getinv()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...) ## Calculate Inverse
  x$setinv(im)
  im
}
