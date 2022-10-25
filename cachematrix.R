## First we make a special function that can cache the inverse of a matrix
## then we we make another one that checks if it is already in cache

makeCacheMatrix <- function(x = matrix()) {
              inv <- NULL
              set <- function(y) {
                          x <<- y
                          inv <<- NULL
   }
              get <- function() x
              setinverse <- function(inverse) inv <<- inverse
              getinverse <- function() inv
              list(set = set, get = get, 
                   setinverse = setinverse,
                   getinverse = getinverse)
}


## check if the inverse matrix is already in cache, if it is display message
## if it is not in cache, then we calculate it

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
              message("getting cached inverse matrix...")
              return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
