## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse property
  inv <- NULL
  set <- function(y) {
    x <<- y 
    inv <<- NULL
  }
  ## Method the get the matrix
  get <- function() x
  ## Way to set the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  ## Way to get the inverse of the matrix
  getinverse <- function() inv
  ## Back the inverse property
  list(set = set, get = get,## Back a list of the methods
       setinverse = setinverse,
       getinverse = getinverse)

}

## Write a short comment describing this function
## Compute the inverse of the unique matrix back by "makeCacheMatrix"
## Back to a matrix  "inv"
cacheSolve <- function(x, ...) {
      
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## Compute the inverse via matrix multiplication
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)## Set the inverse to the object
  inv ## Coming back the matrix
}

## Thank You for ur feedback
