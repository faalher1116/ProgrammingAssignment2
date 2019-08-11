## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## This function creates a special "matrix" object that can cache its inverse
  i <- NULL
  set <- function (y) {   ##Set the matrix
    x <<- y
    i <- NULL
  }
  get <- function() x  ##Get the matrix
  setinv <- function(inv) i <<- inv  ##Set the inverse of the matrix
  getinv <- function() i  ##Get the inverse of the matrix
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if (!is.null(i)){
      message("getting cached data")
      return(i)
  }
  mat <- x$get()
  i <- solve(mat,...)
  x$setinv(i)
  i
}
test <- matrix(c(1:4),nrow = 2, ncol = 2)
object <- makeCacheMatrix(test)
result <- cacheSolve(object)
