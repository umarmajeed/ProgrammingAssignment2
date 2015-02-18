## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly 
## Below are two functions that are used to create a special object that stores a Matrix and cache's its inverse.

#The function , makeCacheMatrix creates a special "Matrix", which is really a list containing functions to
#set the value of the Matrix
#get the value of the Matrix
#set the value of the Inverse of Matrix
#get the value of the Inverse of Matrix
# assume that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {
  
  ## initially setting inverseMAtrix to NULL
  ## <<- operator is used to assign a value to an object in an environment that is different from the current environment
  
  inverseMatrix <- NULL
    
  setMatrix <- function(y) {
      ## changing contents of Matrix X 
      x <<- y
      
      ## Setting InverseMatrix to NULL as Value of Matrix Is changed
      ## so that we need to recompute inverse Again
      inverseMatrix <<- NULL
  }
  
  getMatrix <- function() {
    ## Returning Matrix Set by User
    x
  }
  
  getInverse <- function() {
    ## This functon returns InverseMatrix stored in cache
    ## if InverseMAtrix is Not in cache NULL is returned
    inverseMatrix
  }
  
  setInverse <- function(inverse) {
    ## this function caches Inverse Matrix 
    ## <<- operator is used to assign a value to an object in an environment that is different from the current environment
    inverseMatrix <<- inverse
  }
  
  ## Returning list containing functions to set , get Matrix and Inverse Of Matrix

  list(set = setMatrix,get = getMatrix , setInverse = setInverse , getInverse = getInverse)
}



## cacheSolve function calculates the inverse of the special "Matrix" created with makeCacheMatrix .
## However, it first checks to see if the inverse has already been calculated. 
## If Inverse has already been calculated it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data
## and sets the value of the inverse in the cache via the setInverse function.


cacheSolve <- function(x, ...) {
  
  ## getting Inverse of Matrix from Cache
  inverseMatrix <- x$getInverse()
  
  ## checking if InverseMatrix returned from cache is null 
  if(!is.null(inverseMatrix)) {
    ## if inverseMatrix not null a message is printed and inverse Matrix is returned 
      message("getting cached data for Inverse Matrix")
      return(inverseMatrix)
    }
  
  ## we reach here if InverseMatrix is not present in cache
  ## geting data from specailMatrix 
  data <- x$get()
  
  ## solve() function is use to calculate inverse of Matrix
  inverseMatrix <- solve(data, ...)
  
  ## setInverse function is used to store data in cache 
  x$setInverse(inverseMatrix)
  
  ## Return a matrix that is the inverse of 'x'
  inverseMatrix
  
}
