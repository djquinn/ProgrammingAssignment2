##David Quinn, Prog Assignment No 2
## 2 Functions:
## "makeCacheMatrix" -  creates a "matrix like" object that stores an invertible matrix
##                      and, if subsequently called by cacheSolve, its inverse in global memory
## "cacheSolve" - returns the inverse of matrix portion of the "matrix-like" object stored
##                  with "makeCacheMatrix".  If the inverse was previously computed and stored 
## in memory it will pull the value of the inverse from memory.  Else, it will compute the inverse 
## and store it in global memory.

## Creates a list object that  stores a matrix and, if inverted with cacheSolves,
## stores the inverse in global memory/cache as well

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  setmat <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getmat <- function() x
  setinv <- function(inverse) inv <<- inverse #used to store inverse in global cache/memory
  getinv <- function() inv #pulls inverse from cache when called
  list(setmat=setmat,getmat=getmat,setinv=setinv,getinv=getinv)
}

## Function takes in a "matrix"-like object that is created by a previous call 
# of "makeCacheMatrix" fuction.  If the inverse of the matrix part of the "matrix"-like
# object has been calculated and stored in memory/cache, returns that cached value.
# If not, the function calculates and returns the inverse of the matrix and stores it in
# global cache/memory using the "makeCacheMatrix.setInv()" function
# Assumption: the input object "x" is a "matrix-like" object created by the "makeCacheMatrix"
# function "makeCacheMatrix"
# Assumption: The matrix passed to/created by "makeCacheMatrix" is invertible

cacheSolve <- function(x, ...) {
  inv<-x$getinv() #if inverse is in memory, will return non-NULL value
  if(!is.null(inv)){ #if inverse in memory, return the cached value
    message("Getting cached Inverse data")
    return(inv)
  }
  matrixData<-x$getmat()
  inv<-solve(matrixData,...)
  x$setinv(inv)
  inv
}
