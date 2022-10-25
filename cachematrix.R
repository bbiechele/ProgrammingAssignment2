## First we make a special function that can cache the inverse of a matrix
## then we we make another one that checks if it is already in cache

makeCacheMatrix <- function(x = matrix()) {
              i <- NULL
              set <- function(y) {
                          x <<- y
                          i <<- NULL
   }
              get <- function() x
              setinverse <- function(inverse) i <<- inverse
              getinverse <- function() i
              list(set = set, get = get, 
                   setinverse = setinverse,
                   getinverse = getinverse)
}


## check if the inverse matrix is already in cache, if it is display message
## if it is not in cache, then we calculate it

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
              message("getting cached inverse matrix...")
              return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
