## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function is to make a  cache matrix. 
makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  setMatrix <- function(y){
    x <<- y
    invMatrix <<- NULL
  }
  getMatrix <- function() {x}    ## get a matrix
  setInverse <- function(inverse){invMatrix <<- inverse}  
  getInverse <- function() {invMatrix}     ## get the inverse of the matrix
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## this function is to solve the inverse ofthe matrix
cacheSolve <- function(x, ...) {
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)){
    message("getting cached data")
    return(invMatrix)
  }
  data <- x$getMatrix()
  invMatrix <- solve(data, ...)
  x$setInverse(invMatrix)
  invMatrix
}
