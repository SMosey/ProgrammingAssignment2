

## The following functions are used to cache the inverse of a matrix

## makeCacheMatrix creates a special "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      
      get <- function() x
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      list(set=set, get=get, 
           setInverse = setInverse,
           getInverse = getInverse)
}


## cacheSolve calculates the inverse of the special "matrix" created with the above function. However, it first checks to see if the inverse has alread been calculated. If so, it gets the result from the cache and skips the computation. 

cacheSolve <- function(x, ...) {
      inv <- x$getInverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()   
      inv <- solve(data, ...)  
      x$setInverse(inv)
      inv
}

