## These two functions makeCacheMatrix and cacheSolve work 
## together to allow the user to find the inverse of a matrix 
## and cache the solution, so that the computation is  only 
## carried on the first occasion it is called. 
## This means that the function call to cacheSolve could be placed 
## in a loop without worrying about the execution cost.

## The code takes advantage of R's lexical scoping rules, i.e.,
## the free value used in in a function is taken from the 
## environemnt where the function is defined.  

## Example usage
## z <- matrix (1:4, nrow = 2, ncol =2) # creates 2x2 matrix
## zcache <- makeCacheMatrix (z)    # sets up special matrix
## solved <- cacheSolve (zcache)    # solves z and caches result
## solved <- cacheSolve (zcache)    # uses cached solution
## z [1,1] <- 2                     # Modify original matrix
## zcache$set(z)                    # Updated matrix z in cache
## solved <- cacheSolve (zcache)    # solves new z and caches result


# Tim Perkins 20140612

## The function makeCacheMatrix allows a matrix (and its inverse)
## to be cached. It returns the "matrix" in the form of a list of
## functions. If the original matrix is updated, the cache must be 
## updated using the set() function.

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

## cacheSolve returns the inverse of a matrix
## which is assumed to be invertible. The solution is cached in
## the calling environment for rapid retrieval in case of 
## future calls to this function. 
## As input, it requires a list returned from makeCacheMatrix.
## Additional arguments are passed directly to solve

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getinverse ()
    if (!is.null(s)) {
        message ("cacheSolve: getting cached data")
        return (s)
    }
    data <- x$get()
    s <- solve (data, ...)
    x$setinverse (s)
    s
}
