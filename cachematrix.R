## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      m<-NULL
      setM<-function(y){
            x<<-y
            m<<-NULL
      }
      getM<-function()x
      setMInverse<-function(solve)m<<-solve
      getMInverse<-function()m
      list(setM=setM,getM=getM,setMInverse=setMInverse,getMInverse=getMInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      m<-x$getMInverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$getM()
      m <- solve(data, ...)
      x$setMInverse(m)
      m
}