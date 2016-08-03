## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

      i <- NULL #inverse will be stored to i

      #use reset the "bare" matrix insie of the makeCacheMatrix type and clear the inverse
      set <- function(y){
            x <<- y
            i <<- NULL
      }
      #use to pull out the bare matrix from the makeCacheMatrix type
      get <- function() x
      
      #Setting and getting the inverse
      setinverse <- function(inv) i <<- inv
      getinverse <- function() i
      
      #
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

      i <- x$getinverse()
      
      #If an inverse has been cached, return it
      if(!is.null(i)){
            message("getting cached data")
            return(i)
      }
      
      #Otherwise, calculate the inverse, set it to the cache, and return it
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      ## Return a matrix that is the inverse of 'x'
      i

}
