## Assignment: Caching the Inverse of a Matrix

## This file contains two functions. 
## 1. The function 'makeCacheMatrix' is a special matrix object and 
## returns a list of four functions:
## 2. the second function 'cacheSolve' computes the inverse of a supplied
## special matrix object created by 'makeCacheMatrix' and sets the result 
## in the special matrix object.

## This function creates a special "matrix" object that caches a set inverse. 
## It is assumed, that the matrix supplied is always invertible.
## The return object is a list of functions:
## 1. set: set the matrix
## 2. get: get the matrix
## 3. setInverse: set the inverse
## 4. getInverse: get the cached inveres
makeCacheMatrix <- function(x = matrix()) {
  ## Initialize variable inv with NULL
  inv <- NULL
  ## set matrix and reset variable inv
  set <- function(matrix = matrix()) {
    x <<- matrix
    inv <<- NULL
  }
  ## Return matrix
  get <- function() x
  ## Return inverse of x
  getInverse <- function() inv
  ## set inverse of m
  setInverse <- function(inverse = matrix()) inv <<- inverse
  ## return list of functions
  list(set=set, get=get, getInverse=getInverse, setInverse=setInverse)
}

## This function computes the inverse of the special "matrix" returned 
## by #makeCacheMatrix'. If the inverse has already been calculated (and the 
## matrix has not changed), then 'cachesolve' function retrieves the inverse from 
## the cache and returns that value.
cacheSolve <- function(x, ...) {
  ## check for already computed inverse
  i <- x$getInverse()
  ## if inverse already exists, return it
  if(!is.null(i)) {
    message("getting cached data")
    ## return cached inverse
    return(i)
  }
  ## otherwise compute and set (=cache) inverse
  x$setInverse(solve(x$get()))
  ## and return the inverse
  x$getInverse()
}
