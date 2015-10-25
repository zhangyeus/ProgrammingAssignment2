## These two functions implement cache mecanism for 
## inversing matrix

## Example:
## > a<-makeCacheMatrix(rbind(c(1,-0.25), c(-0.25, 1)))
## > cacheSolve(a)
##          [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## This function returns a data structure that contains
## both the matrix (to be inversed) and four helper methods 

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


## This function returns the inverse of the cache matrix.
## If the inverse has already being computed, then it
## directly returns the inersed matrix from cache.
## Otherwise it computes the inverse and cache it.

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
