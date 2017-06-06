## makeCacheMatrix and cacheSolve functions are used to create 
## a special object that stores
## a numeric matrix and cache's its inverse.

## This function creates a special "matrix", 
## which is a list of functions containing:

## set: set the elements of the matrix
## get: get the elements of the matrix
## setinverse: set the elements of the inverse
## getinverse: get the elements of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() return(m)
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## This function calculates the inverse of the special "matrix" 
## created with makeCacheMatrix function.
## First, it checks if the inverse has already been calculated.
## If this is true, then it gets the inverse from the cache and 
## skips computation.
## Otherwise, it calculates the inverse of the data and sets the result
## of the inverse matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


## To test the result of function use the following code:
m <- matrix(c(4, 1, 3, 1), nrow = 2, ncol = 2)
cm <- makeCacheMatrix(m)
inversed_m <- cacheSolve(cm)
## "getting cached data" output is expected.
inversed_m2 <- cacheSolve(cm)

## Check if inverse matrix is correct.
test_inverse <- m %*% inversed_m
test_inverse2 <- inversed_m %*% m

