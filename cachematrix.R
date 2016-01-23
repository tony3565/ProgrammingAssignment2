## These two functions cache the value of the inverse of a
## matrix, such that once computed, subsequent requests (like
## in a loop) to calculate the inverse will save computation 
## time by recalling the cached value. I mean, what else 
## could you do but move the command out of the loop or
## store the inverse in a variable, 

## makeCacheMatrix creates a special "matrix", which is really 
## a list containing a function to set the value of the matrix, 
## get the value of the matrix, set the value of the matrix to 
## the inverse of the matrix, and get the value of the inverse 
## of the matrix.
makeCacheMatrix <- function (x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## The following function calcuates the inverse of the special
## "matrix" created with the makeCacheMatrix function. Function
## cacheSolve checks for a prior computation, returning the cached
## result if found. Otherwise, the inverse is computed and returned.
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
