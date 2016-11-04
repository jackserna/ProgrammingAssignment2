## The following code will create matrix inversions, and instead of computing 
## repetitively everytime, I will write one function to compute a matrix inversion 
## and save that "matrix" object into a different environment as a cache. I will 
## write another function to solve the inversion by first checking the cache, and 
## if not, then a new matrix inversion will be created and saved in the cache. 

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                # Use <<- to assign a value to an object outside current environment
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not 
## changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        # If the inverse is cached
        if(!is.null(m)) {
                #get it from the cache and skip computation
                message("getting cached data")
                return(m)
        }
        # If the inverse is not cached, compute inverse
        data <- x$get()
        m <- solve(data, ...)
        # Set the value of the inverse in the cache
        x$setsolve(m)
        return(m)
}