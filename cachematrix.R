## Put comments here that give an overall description of what your
## functions do
##
## These functions work together to implement matrix inverse caching:
## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## cacheSolve computes the inverse of the matrix, using cached values when available
## This caching mechanism helps avoid repeated costly computations of matrix inversions

## This function creates a special "matrix" object that can cache its inverse.
## It returns a list of functions to:
## 1. set the matrix value
## 2. get the matrix value
## 3. set the inverse
## 4. get the inverse
makeCacheMatrix <- function(x = matrix()) {
  # initialize the matrix inverse to NULL
  inverse <- NULL
  
  # set the matrix and reset the inverse cache
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  # get the matrix
  get <- function() {
    x
  }
  
  # set the inverse of the matrix
  setinverse <- function(inv) {
    inverse <<- inv
  }
  
  # get the inverse of the matrix
  getinverse <- function() {
    inverse
  }
  
  # return a list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" object
## created by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then it retrieves the
## inverse from the cache to avoid redundant computation.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  
  # if inverse is cached, return it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  # get the matrix and compute its inverse
  data <- x$get()
  inverse <- solve(data, ...)
  
  # cache the inverse
  x$setinverse(inverse)
  
  # return the inverse
  inverse
}

## Test the implementation
# Create a matrix
m <- matrix(c(1,2,3,4), nrow=2, ncol=2)
print("Original matrix:")
print(m)

# Create a special matrix object
special_matrix <- makeCacheMatrix(m)

# First time computing inverse (should compute and cache)
print("\nComputing inverse first time:")
inverse1 <- cacheSolve(special_matrix)
print(inverse1)

# Second time getting inverse (should retrieve from cache)
print("\nGetting inverse second time (should use cache):")
inverse2 <- cacheSolve(special_matrix)
print(inverse2)

# Verify the result by multiplying matrix with its inverse
print("\nVerifying result (should be identity matrix):")
print(m %*% inverse2)

# Change the matrix and compute new inverse
print("\nChanging matrix and computing new inverse:")
special_matrix$set(matrix(c(2,3,4,5), nrow=2, ncol=2))
print("New matrix:")
print(special_matrix$get())
print("New inverse:")
print(cacheSolve(special_matrix))
