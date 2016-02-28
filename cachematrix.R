## makeCacheMatrix: Creates a special 'matrix' object that can cache its inverse
## cacheSolve: Computes the inverse of a matrix (or returns a cached version)

## This function creates a special "matrix" object that can cache its inverse
## As no calculations occur here, this is essentially the same as makeVector
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## If the inverse has already been calculated (and the matrix has not changed), 
## then retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  
  ## First, check to see if the inverse of x has been cached
  ## If so, retrieve and return that matrix
  if(!is.null(m)) {
    message("Getting cached matrix...")
    return(m)
  }
  
  ## If x inverse has not been cached, inverse and cache it
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}