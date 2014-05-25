## Matrix inversion is usually a costly computation and their may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly.
## If the contents of a matrix are not changing, it will cache the value of the inverse,
## it could looked up in the cache rather than recomputed.

## makeCacheMatrix : this function creates a special "matrix" object that can cache its inverse.
## the function is containing:
## 1. set the value of the matrix
## 2. get the value of the vector
## 3. set the value of its inverse.
## 4. get the value of its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## cacheSolve : this function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
