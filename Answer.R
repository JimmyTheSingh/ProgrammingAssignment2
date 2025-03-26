## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#function creates a matrix to cache the inverse
makeCacheMatrix <- function(x = matrix()) 
  {
  
  #intializes the matrix to be NULL so values can be added
  matInverse <- NULL
  
  set <- function(y) 
    {
    x <<- y
    matInverse <<- NULL
    }
  
  get <- function() x
  
  setmatInverse <- function(inv) matInverse <<- inv
  
  getmatInverse <- function() inverse
  
  list(set = set, get = get, setmatInverse = setmatInverse, getmatInverse = getmatInverse)

  
#calculates the inverse of the matrix returned in the prev function
cacheSolve <- function(x, ...) 
  #this will return the inverse of x
  {
  matInverse <- x$getmatInverse()
  
  if(!is.null(matInverse)) 
  {
    message("getting cached data")
    return(matInverse)
  }
  
  data <- x$get()
  matInverse <- solve(data, ...)
  x$setmatInverse(matInverse)
  matInverse
  }