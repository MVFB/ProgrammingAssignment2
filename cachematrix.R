## Cache the inverse of a matrix


## Creates a list that checks if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the result in the cache 

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
		set <- function(y) {
				x <<- y
				m <<- NULL
		}
		get <- function() x
		setSolve <- function(Solve) m <<- Solve
		getSolve <- function() m
		list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## The following function calculates the inverse.
## It first checks to see if the inverse has already been calculated.
## If so, it gets the result from the cache and skips the computation.
## Otherwise, it calculates the inverse and sets the result in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getSolve()
        if(!is.null(m)) {
        		message("getting chached data")
        		return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setSolve(m)
        m
}
