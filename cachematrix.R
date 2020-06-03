## Put comments here that give an overall description of what your
## functions do

## This function makes a cache matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL                             ##assign value null to inverse
  set <- function(y) {                        ##define function 'set'
    x <<- y                                   ##assign y to x
    inv <<- NULL                              ##assign value null to inv
  }
  get <- function() x                              ##define function 'get'
  setInverse <- function(inverse) inv <<- inverse  ##set inverse
  getInverse <- function() inv                     ##get inverse
  list(set = set,                                  ##create list
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the matrix input 
## If the inverse was already computed then it should retrieve
## the inverse from the cache that was created when it was first computed

cacheSolve <- function(x, ...) {    ##retrieve cached inverse matrix
  inv <- x$getInverse()             ##get inverse
  if (!is.null(inv)) {              ##get cached data if is exists
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()                    ##
  inv <- solve(mat, ...)            ##invert matrix
  x$setInverse(inv)                 ##set inverse
  inv                               ##output inverse
}
