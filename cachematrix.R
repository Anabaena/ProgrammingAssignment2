## This program allows you to store a matrix in the cache using
## "makeCacheMatrix", while the "cacheSolve" function subsequently
## calculates the inverse of said matrix (and stores it in the cache),
## unless the inverse of the matrix already exists
##
## Written by: Joelle Jansen


## First makeCacheMatrix is called to load the matrix and make the
## set, get, setinverse and getinverse functions available for usage
## Use: $myMatrix <- makeCacheMatrix(matrix)
## And then you can use the listed functions like this:
##	  $myMatrix$get()

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) inverse <<- solve
	getinverse <- function() inverse
	list(set = set, get = get, setinverse = setinverse, 
	getinverse = getinverse)
}


## The cacheSolve function then calculates the inverse, unless it 
## already exists

cacheSolve <- function(x, ...) {
   	##Check whether matrix inverse is already present in cache
	inverse <- x$getinverse()
	if (!is.null(inverse)) {
		message("getting cached inverse")
		return(inverse)
	}
	##If not, calculate inverse
	data <- x$get()
	inverse <- solve(data)
	x$setinverse(inverse)
	inverse
		
}
