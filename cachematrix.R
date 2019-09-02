## A pair of functions that together can potentially save time on the costly operation of
## inverting a matrix. They define an object that can store and retrieve both the original
## and the inverted matrix, as well as another function that will make the calls to that
## object appropriately.

## Object that can store and retrieve a matrix as well as its inverse.
## If a new matrix is stored to it, then the cached inverse will be cleared.

makeCacheMatrix <- function(x = matrix()) {
	invMat <- NULL
	set <- function(foo) {
		x <<- foo
		invMat <<- NULL
	}
	get <- function() x
	setInverse <- function(bar) invMat <<- bar
	getInverse <- function() invMat
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Try to get the cached inverse matrix from our object x.
## If we can get it, then just return that result to x.
## Otherwise, compute the inverted matrix and cache that result to x.

cacheSolve <- function(x, ...) {
	invMat <- x$getInverse()
	if(!is.null(invMat)) {
		message("Getting cached data.")
		return(invMat)
	}
	data <- x$get()
	invMat <- solve(data, ...)
	x$setInverse(invMat)
	invMat
}
