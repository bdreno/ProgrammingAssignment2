## Coursera R Programming Assignment #2
## 
## Usage: source("/path/to/repository/ProgrammingAssignment2/cachematrix.R")
## This document contains the two functions for the 
## assignment.  One to convert a matrix to a version that allows
## for caching of its inverse, and another to either retrieve or calculate
## the inverse as necessary

## Creates a matrix capable of caching its inverse.  The new matrix has functions for
## setting and getting data as well as setting and getting the Inverse
makeCacheMatrix <- function(x = matrix()) {
	cachedInverseMatrix <- NULL
	set <- function(y){
		x <<- y
		## Reset the cached inverse so its is recalculated if the matrix is ever changed
		cachedInverseMatrix <<-NULL
	}
	get <- function() x
        setInverse <- function(inverseMatrix) cachedInverseMatrix <<- inverseMatrix
        getInverse <- function() cachedInverseMatrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Calculate or retreive the inverse of a matrix processed through makeCacheMatrix()
## The inverse is only calculated if the cached version is NULL
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
                message("retrieving cached inverse matrix")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}


