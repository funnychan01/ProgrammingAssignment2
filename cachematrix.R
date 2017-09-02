## The following pair of functions is for caching the inverse of matrix.

## The first function, makeCacheMatrix, creates a special "matrix" object that 
## can cache its inverse.

## It contains 2 objects:
## x with default argument for writing in the input matrix, and 
## inv_mat set to be null for storing inverse matrix.

## Also, it contains 4 functions:
## the set function is for writing in new matrix, 
## the get function is for displaying the updated matrix, 
## the setinverse function is for caching inverse matrix from the cacheSolve function below, 
## the getinverse function is for retrieving inverse matrix.

## A list with 4 named elements will be created, so that
## it's more convenient use $ to retrieve the elements from the list.

makeCacheMatrix <- function(x = matrix()) {
  inv_mat <- NULL
  set <- function(y) {
    x <<- y
    inv_mat <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv_mat <<- inverse
  getinverse <- function() inv_mat
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The second function, cacheSolve, computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then 
## the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_mat <- x$getinverse()
  if(!is.null(inv_mat)) {
    message("getting cached data")
    return(inv_mat)
  }
  data <- x$get()
  inv_mat <- solve(data, ...)
  x$setinverse(inv_mat)
  inv_mat
}
