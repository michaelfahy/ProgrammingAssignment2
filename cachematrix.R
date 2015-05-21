## makeCacheMatrix and cacheSolve are a pair of functions that cache the inverse of a matrix

## This "makeCacheMatrix" function creates a special "matrix" object 
## that can cache its inverse.

## It actaully returns a lis of four functions that can:

##  1.  set the value of the matrix
##  2.  get the value of the matrix
##  3.  set the value of the inverse matrix
##  4.  get the value of teh inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  mi <- NULL                                 # the "mi" varilable will contain the matrix inverse
  set <- function(y) {                       # the set function changes teh matrix
    x <<- y                                  # set the value of x in thecalling environment to y
    mi <<- NULL                              # reset the cahced inverse to NULL
  }
  get <- function(){                         # returns the matrix
    x
  } 
  setinverse <- function(inverse) {          # sores teh inverse in the cache    
    mi <<- inverse
  }
  getinverse <- function() {                 # returns the inverse from the cache
    mi
  }
  list(set = set, get = get,                 # creates a list of functions to be returned
       setinverse = setinverse,
       getinverse = getinverse)
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed),
##  then the cachesolve function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mi <- x$getinverse()                     # Try to get a cached inverse
  if(!is.null(mi)) {                       # If there is a cached inverse       
    message("getting cached data")
    return(mi)                              # return it
  }
  data <- x$get()                           # If there is no cached inverse, get the matrix
  mi <- solve(data, ...)                    # Calculate its inverse and store it in the mi variable
  x$setinverse(mi)                          # Put the inverse in the cache
  mi                                        # return the inverse
}
