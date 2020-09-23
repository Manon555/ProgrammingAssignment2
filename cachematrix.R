## Below are two functions that are used to create a special object that stores a numeric matrix and caches its inverse
## functions do

## makeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL 
    set <- function(y) {
        x <<- y 
        i <<- NULL 
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve is a function that computes the inverse of the special "matrix" returned by the makeCacheMatrix function defined above.
## If the inverse has already been calculated (and the matrix has not changed), the cachesolve function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i   
}

## Some testing with an invertible matrix

x <- makeCacheMatrix(matrix(c(1,0,3,2,2,4,3,2,1),ncol=3))
cacheSolve(x) # the inverse is freshly calculated
cacheSolve(x) # the inverse is taken from the cached inverse matrix value (message "getting cached data" is displayed)