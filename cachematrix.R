## cacheSolve and makeCacheMatrix functions are used to cache the inverse of a matrix

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) { # define 1st function: SET
                x <<- y 
                inv <<- NULL
        }
        get <- function() x # define 2ND function: GET
        setinv <- function(c_inv) inv <<- c_inv # define 3rd function: SETINV
        getinv <- function() inv # define 4th function: GETINV
        list(set = set, get = get, # return "list" of functions, to be accessed e.g. with "$set"
             setinv = setinv,
             getinv = getinv)
}

## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), it retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...) ## solve is the function to return the inverse of a matrix
        x$setinv(inv)
        inv
}