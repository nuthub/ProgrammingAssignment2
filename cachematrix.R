## Assignment: Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse. 
## It is assumed, that the matrix supplied is always invertible.
makeCacheMatrix <- function(m = matrix()) {
  ## Initialize variable inv with NULL
  inv <- NULL
  ## set matrix and reset variable inv
  set <- function(matrix = matrix()) {
    m <<- matrix
    inv <<- NULL
  }
  ## Return matrix
  get <- function() m
  ## Return inverse of m
  getInverse <- function() inv
  ## set inverse of m
  setInverse <- function(inverse = matrix()) inv <<- inverse
  ## return list of functions
  list(set=set, get=get, getInverse=getInverse, setInverse=setInverse)
}

## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from 
## the cache.
cacheSolve <- function(m, ...) {
  ## check for already computed inverse
  i <- m$getInverse()
  ## if inverse already exists, return it
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## otherwise compute and set (cache) inverse
  m$setInverse(solve(m$get()))
  ## and return the inverse
  m$getInverse()
}
