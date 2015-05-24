# R Programming Week 3
#Author: Cristian Gómez Alvarez <cristianpark@gmail.com>
#

#Function to save the inverse of a matrix in cache so the process haven't to be executed any time
makeCacheMatrix <- function(matrixObj=matrix()) {
	#Attributes
    inverse <- NULL
    
    #Functions
    setMatrix <- function(matrixObjNew) {
			matrixObj <<- matrixObjNew
			inverse <<- NULL
	}
	getMatrix <- function() matrixObj
	setInverse <- function(inverseNew) inverse <<- inverseNew
	getInverse <- function() inverse
	
	#CacheMatrix special list with attributes and functions relative to cache results of the inverse
	list(setMatrix = setMatrix, getMatrix = getMatrix,
		 setInverse = setInverse,
		 getInverse = getInverse)
}

#Function to return the inverse of a matrix
cacheSolve <- function(cacheMatrix, ...) {
        matrixInv <- cacheMatrix$getInverse()
        
        if(!is.null(matrixInv)) {
                message("Getting inverse from Cache...")
                return(matrixInv)
        }
        
        matrixList <- cacheMatrix$getMatrix()		#Get the matrix from cachaMatrix Object
        matrixInv <- solve(matrixList, ...)			#Perform the inverse calculation
        cacheMatrix$setInverse(matrixInv)			#Cache the result from the Matrix Inverse
        
        matrixInv
}
