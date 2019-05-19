
##These functions were produced to meet requirements of R Programming Assignment 2
## R Programming course; Week 3
##They type of functions can be used to cache potentially time-consuming (repeated) computations

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {  
  inv <- NULL                                
  set <- function(y) {                        
    x <<- y                                 
    inv <<- NULL                            
  }
  get <- function() x                        
  
  setinverse <- function(inverse) inv <<- inverse  
  getinverse <- function() inv                     
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)   
}
                                                                                
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

  cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
  }
