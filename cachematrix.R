
## Methods to calculate the inverse of a matrix, results are cached 
## so that if the same matrix is calculated again its inverse is fetched from the cache

## Creates a special matrix object to cache matrices
## Has set/get functions for the value of the matrix and for the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}


## Calculates the inverse of a matrix
## The result is stored in a special cache object, 
## if a matrix inverse has already been calculated the result is fetched from the cache
## Otherwise the matrix inverse is calculated and stored in the cache
## Finally returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m        
}