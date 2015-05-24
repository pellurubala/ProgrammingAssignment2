## *****************************************************************************
## Below are two functions that are used to create an object that stores a matrix and 
## cache's its inverse value.
## *****************************************************************************


## *****************************************************************************
## The function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to:
## set the value of the matrix : setMatrix
## get the value of the matrix : getMatrix
## set the value of the inverse : setInverse
## get the value of the inverse : getInverse
## *****************************************************************************
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL 					## assigning 'NULL' to inverse
	setMatrix <- function(y) {			
		x <<- y 					## Setting the matrix 'x'
		inverse <<- NULL
	}
	getMatrix <- function() x 				## Returning matrix 'x'
	setInverse <- function(solve) inverse <<- solve 	## Cache the value of the inverse 
	getInverse <- function() inverse 			## Returning inverse
	list(setMatrix = setMatrix, getMatrix = getMatrix,
	     setInverse = setInverse,
	     getInverse = getInverse)
}


## *******************************************************************************************************
## The function, cacheSolve calculates the inverse of the special "matrix" created with the above function.
## it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache  
## using the 'setInverse' function.
## *******************************************************************************************************


cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()				## Getting inverse
	if(!is.null(inverse)) {					## Checking for the presence of inverse :Yes
		message("getting cached data")		
		return(inverse)
	}
	data <- x$getMatrix()					## Getting Matrix : if no control comes here
	inverse <- solve(data, ...)				## Using solve() to compute inverse
	x$setInverse(inverse)					## To cache the inverse
	inverse 						## Returning the inverse
}
