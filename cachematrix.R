## Put comments here that give an overall description of what your
## functions do
#The functions will cache the inverse of a matrix.
## Write a short comment describing this function
#makeCacheMatrix is a function that will create an object that can cache its inverse for the input
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Write a short comment describing this function
# cacheSolve is a function that will compute the inverse of the returned by makeCacheMatrix. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  } 
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
