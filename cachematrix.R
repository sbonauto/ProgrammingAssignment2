## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this code contains functions: makeCacheMatrix and cacheSolve
##makeCacheMatrix includes set, get, setinverse, getinverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL  ##inverse initialized as NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function(x). ##gets matrix x
	setinverse <- function(inverse) inv <- inverse
	getinverse <- function() inv ##obtains inverse
	list (set = set, get = get,
			setinverse = setinverse, 
			getinverse = getinverse) 
}


## Write a short comment describing this function
## this function is used to get the cache data

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
        	message ("getting cached data")
        	return (inv)
        }
        matrix_to_invert <- x$get()
        inv <- solve(matrix_to_invert, ...)
        x$setinverse(inv)
        inv
}
