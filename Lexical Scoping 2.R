## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# First clear the R environment
rm(list=ls())

#set my working directory to coursera files on desktop
setwd('C:/Users/andos/Desktop/datascience coursera quiz/rprog-data-specdata')

#practice on the example "makeVector" and "CacheMean"
#practice #1 using makeVector
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

myMakeVectorVar <- makeVector(1:15)

#practice #2 using cacheMean
cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

myMeanVectorVar <- makeVector(1:15)

makeVector(c(1:15))
cachemean(makeVector(c(1:15)))


#Now to the Assignment
makeCacheMatrix <- function(x = matrix()) {
  
  matrixInverseVar <- NULL   #set the default matrix inverse to null
  
  set <- function(y) {
    x <<- y
    matrixInverseVar <<- NULL
  }
  
  get <- function() x
  setInverseMatrix <- function(inverse) matrixInverseVar <<- inverse
  getInverseMatrix <- function() matrixInverseVar 
  list(set = set, get = get, 
       setInverseMatrix = setInverseMatrix, 
       getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  kCache <- x$getInverse()
  if(!is.null(kCache)){
    message("getting cached data")
    return(kCache)
  }
  mat <- x$get()
  kCache <- solve(mat,...)
  x$setInverse(kCache)
  kCache
}



This assignment will be graded via peer assessment.
# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # This variable will hold the cached inverse of the matrix
  
  # This function sets the value of the matrix and invalidates the cached inverse
  set <- function(y) {
    x <<- y  # Assign the input matrix to the variable x in the parent environment
    inv <<- NULL  # Reset the cached inverse, as the matrix has changed
  }
  
  # This function returns the value of the matrix
  get <- function() {
    x  # Return the matrix
  }
  
  # This function sets the value of the cached inverse
  setInverse <- function(inverse) {
    inv <<- inverse  # Assign the input inverse to the variable inv in the parent environment
  }
  
  # This function returns the value of the cached inverse
  getInverse <- function() {
    inv  # Return the cached inverse
  }
  
  # Return a list of all the functions defined above
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed),
# then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Attempt to get the cached inverse
  
  # If the cached inverse is not NULL, return it
  if (!is.null(inv)) {
    message("getting cached data")  # Print a message indicating that cached data is being used
    return(inv)  # Return the cached inverse
  }
  
  # Get the matrix from the special "matrix" object
  mat <- x$get()
  
  # Compute the inverse of the matrix
  inv <- solve(mat, ...)
  
  # Cache the computed inverse for future use
  x$setInverse(inv)
  
  # Return the computed inverse
  inv
}
