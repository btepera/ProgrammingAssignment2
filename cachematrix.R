## These functions allow you to cache a matrix and its inverse,
## rather than compute them again

## creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        get<- function() x
        setinv <- function(inv) inverse <- inv
        getinv <- function() inverse
        list(set = set, 
             get = get,
             setinv = setinv,
             getinv = getinv
             )
}


## computes the inverse of the special matrix returned by makeCacheMatrix,
## or retrieves it from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinv()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinv(inverse)
        inverse
}
