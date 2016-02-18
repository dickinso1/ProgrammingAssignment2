## The makeCacheMatrix stores the calculated inverse value of a matrix from the cacheSolve function

## The cacheSolve function checks to see if the calculated inverse matrix is in the cache.  If so, it returns it,
## otherwise, it calculates the inverse matrix and then sets the value through the makeCacheMatrix function.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
##above. If the inverse has already been calculated (and the matrix has not changed), then 
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()                   ## Get inverse matrix value currently stored in cache matrix.
    
    if(!is.null(inv)) {                 ## Check to see if the inverse has already been calculated and stored in cache
        message("getting cached data")
        return(inv)
    }
    mat.data <- x$get()                 ## If not stored in cache, get matrix data.
    inv <- solve(mat.data, ...)         ## use matrix data to solve for the inverse
    x$setinv(inv)                       ## Set inv in cache matrix
    return(inv)
}
