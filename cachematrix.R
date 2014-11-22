## The function makeCacheMatrix() create a list of functions to 
## store the vales of a matrix and it's inverse. cacheSolve() finds the inverse
## of the matrix stored in the list created by makeCacheMatrix(). If the inverse
## has already been computed, then the cached value is returned. If the inverse
## has not been computed, the cacheSolve() computes the inverse and caches the value

## This function creates a list of functions which can be usde to:
## 1) set the values of a matrix 
## 2) get the values of a matrix
## 3) set the inverse of the matrix
## 4) get the inverse of the matrix

## Example: 
## Create matrix: c <- matrix(c(1.00, -0.25, -0.25, 1.00), nrow = 2,ncol = 2)

## Call the function with matrix as input argument: 
##    foo <- makeCacheMatrix(c) 
##
## OR call the makeCacheMatrix to generate a list of functions and then set
## the value of the matrix:
##
##      foo <- makeCacheMatrix()      #create list of functions
##      foo$get()
##
##  returns:
##        [,1]
##        [1,]   NA
##
## Then:: 
##      foo$set(c)                    #set value of matrix
##      foo$get()
##
##  returns:
##
##        [,1]  [,2]
##  [1,]  1.00 -0.25
##  [2,] -0.25  1.00
 
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  class(m)
  
  #function 1) set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #function 2) return the value of the matrix
  get <- function() x
  
  #function 3) set the inverse of the matrix
  setinverse <- function(solve) m <<- solve
 
  #function 4) return the inverse of the matrix 
  getinverse <- function() m
  
  #create and return a list of the above functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Use Solve to find the inverse of a matrix and save the result.
## Input: list of matrix functions generated with makeCacheMatrix.
## Output: If the inverse of the matrix has already been performed, use 
## the value cached in eg. foo$getinverse() otherwise determine the inverse of
## function and cache the value.

## Example:
## c <- matrix(c(1.00, -0.25, -0.25, 1.00), nrow = 2,ncol = 2)
## foo <- makeCacheMatrix(c) 
## foo2 <- cacheSolve(foo)
## foo2$getinverse()

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  #if getinverse does not return null then the inverse of the matrix
  #has already been computed, therefore return the cached value
  if(!is.null(m)) {
    message("Used cached data for matrix inverse")
    return(m)
  }
  
  # if getinverse() is empty the inverse of the matrix has not yet
  # been computed. Get the matrix...
  data <- x$get()
  
  # ...and compute the inverse
  message("Matrix inverse not cached. Computing inverse.")
  m <- solve(data, ...)
  
  #cache the inverse of the matrix
  x$setinverse(m)
  
  #return the inverse of the matrix
  m
  
}
