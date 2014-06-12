## These two functions makeCacheMatrix and cacheSolve work 
## together to allow the user to find the inverse of a matrix 
## and cache the solution, so that the computation is  only 
## carried on the first occasion it is called. 
## This means that the function call to cacheSolve could be placed 
## in a loop without worrying about the execution cost.

## Example usage
## z <- matrix (1:4, nrow = 2, ncol =2) # creates 2x2 matrix
## zcache <- makeCacheMatrix (z)    # sets up special matrix
## solved <- cacheSolve (zcache)    # solves or uses cached if availble

# Tim Perkins 20140605

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function (y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function (solve) inv <<- solve
    getinverse <- function () inv
    list (set = set, 
          get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getinverse ()
    if (!is.null(m)) {
        message ("getting cached data")
        return (m)
    }
    data <- x$get()
    m <- solve (data, ...)
    x$setinverse (m)
    m
}
