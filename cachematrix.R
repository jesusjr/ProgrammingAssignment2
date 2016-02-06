# ## Put comments here that give an overall description of what your functions do
# Author Jesus Junior Garcia Garza
# Usage
# Load the script to enable the functions in your session
# > source("cachematrix.R")
# Define a Matrix
# > m <- matrix(1:4, nrow = 2, ncol = 2)
# > m
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# Call makeCacheMatrix(x)
# > b <- makeCacheMatrix(m)
# Call cacheSolve to get the inverse Matrix for the first time and save it in memory
# > cacheSolve(b)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# If we called again will use the cache stored in memory
# > cacheSolve(b)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5



# The first function, makeCacheMatrix creates a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i<- NULL
  set<-function(y) {
    x<<-y
    i<<-NULL
  }
  get<-function() x
  setinverse <- function(inverse) i <<-inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# This function returns the inverse of a Matrix and stores the same in cache, so if the function is called twice with the same value
# it will return the cache value stored in memory

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <-x$get()
  i<-solve(data)
  x$setinverse(i)
  i
}
