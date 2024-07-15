## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# First clear the R environment
rm(list=ls())

#set my working directory to Coursera files on the desktop
setwd('C:/Users/andos/Desktop/datascience coursera quiz/rprog-data-specdata')

#Now to the Assignment
# This function will create a matrix object called makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) {
  
  matrixInverseVar <- NULL   #set the variable that will hold the matrix inverse, and set the default to null 

  # This will sets the value of the matrix x and also reset the cached inverse to null
  set <- function(y) {
    x <<- y
    matrixInverseVar <<- NULL
  }
  
  get <- function() x
  setInverseMatrix <- function(inverse) matrixInverseVar <<- inverse

  # This returns the value of the cached matrix inverse
  getInverseMatrix <- function() matrixInverseVar 

  # Return all of the above functions as a list
  list(set = set, get = get, 
       setInverseMatrix = setInverseMatrix, 
       getInverseMatrix = getInverseMatrix)
}

# This computes the inverse of the matrix returned by makeCacheMatrix function above
cacheSolve <- function(x, ...) {
 
  kCache <- x$getInverse()       # Return a matrix that is the inverse of 'x'

  # return the cached inverse matrix if it is not NULL
  if(!is.null(kCache)){
    message("getting cached data")
    return(kCache)
  }
  
  mat <- x$get()              # Get the matrix from the makeCacheMatrix
  kCache <- solve(mat,...)    # Compute the matrix inverse
  x$setInverse(kCache)
  kCache                     # Return the inverse answer
}
