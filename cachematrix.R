## Pair of functions to cache the inverse of a square matrix
## Given that the matrix supplied is invertible, 
## hence no check included for invertibility
## Written by: P. Kohli
## 11/20/2018
## R Programming Coursera Assignment week-3


## Function1: makeCacheMatrix function provides a matrix 
## object to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  mat.inverse <- NULL
  set <- function(y) {
    x <<- y
    mat.inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) mat.inverse <<- inverse
  getinverse <- function() mat.inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function2: cacheSolve function returns the inverse of the matrix  
## object 'x' returned by makeCacheMatrix function and if the inverse already 
## exists, then cacheSolve function simply retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
    mat.inverse <- x$getinverse()
    if(!is.null(mat.inverse)) {
      message("getting cached data")
      return(mat.inverse)
    }
    mat <- x$get()
    mat.inverse <- solve(mat, ...)
    x$setinverse(mat.inverse)
    mat.inverse
  }
