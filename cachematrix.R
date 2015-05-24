##makeCacheMatrix takes a normal R matrix and creates an object that is capable of storing
##a cached inverse of the given matrix.
##Example usage: x <- makeCacheMatrix(inputMatrix)
##The above line will make object x, with get and set functions for the given matrix, as well as
##for the inverse.  Also, x$getchanged() can be used to obtain a TRUE or FALSE value for whether
##the matrix has been changed since the inverse was last calculated.

makeCacheMatrix <- function(x = matrix()){
	inv <- NULL
	changed <- FALSE
	set <- function(y){
		changed <<- TRUE
		x <<- y
	}
	get <- function() x
	getinverse <- function() inv
	setinverse <- function(inverse){ 
		changed <<- FALSE
		inv <<- inverse
	}
	getchanged <- function() changed
	list(set = set, get = get, getinverse = getinverse, setinverse = setinverse, getchanged = getchanged)
}

##cacheSolve takes a CacheMatrix created by the makeCacheMatrix function and returns the inverse.
##If the inverse has already been found for the current matrix, it is returned from cache instead of 
##recalculating.

cacheSolve <- function(x, ...){
	inv <- x$getinverse()
	ischanged <- x$getchanged()
	if(!is.null(inv) && !ischanged){
		message("getting cached inverse")
		return(inv)
	}
	calc <- x$get()
	inverse <- solve(calc)
	x$setinverse(inverse)
	inverse
}
