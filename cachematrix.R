## Functions aimed at caching the inverse of a matrix to save on computational time
## Limitation that the matrix supplied must be invertible

## makeCacheMatrix establishes the matrix and creates an object that can cache the matrix inverse 
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve calculates the inverse of a matrix established in makeCacheMatrix
## if the inverse has already been solved, then it is retrieved from the cache, else it is calculated and returned
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinverse(inv)
        inv        
}
