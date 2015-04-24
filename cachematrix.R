## The following functions compute and cache the inverse of the matrix. 
## If the cache version is available, its returned immedidately. 
## If the cache version is not available, the inverse is computed and then 
## cached

## Create the matrix object and cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(invval) inv <<- invval
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Computes the inverse of the matrix if the cache version is not available.
## If the cache version is available, it retrieves the inverse from cache.

cacheSolve <- function(x, ...) {
	m <- x$getinv()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	mat <- x$get()
        m <- solve(mat)
 	x$setinv(m)
	m
}
