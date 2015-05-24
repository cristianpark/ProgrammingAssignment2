# R Programming Week 3
# Functions to calculate inverse of a matrix fetching from cache when possible
#
# Author: Cristian Gómez Alvarez <cristianpark@gmail.com>
#
## Usage example:
## > source("ProgrammingAssignment2/cachematrix.R")
## > matriz<-matrix(c(2,3,5,0,0,1,1,0,1), 3, 3)
## > matrixEj<-makeCacheMatrix(matriz)
## > inversa<-cacheSolve(matrixEj)
## > inversa
##	[,1]       [,2] [,3]
##	[1,]    0  0.3333333    0
##	[2,]   -1 -1.0000000    1
##	[3,]    1 -0.6666667    0
## > inversa<-cacheSolve(matrixEj)
## 	Getting inverse from Cache...
#

#Function to save the inverse of a matrix in cache so the process doesn't have to be executed any time
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
	
	#List with attributes and functions relative to cache results of the inverse
	list(setMatrix = setMatrix, getMatrix = getMatrix,
		 setInverse = setInverse,
		 getInverse = getInverse)
}

#Function to return the inverse of a matrix (from cache if possible)
cacheSolve <- function(cacheMatrix, ...) {
        matrixInv <- cacheMatrix$getInverse()
        
        if(!is.null(matrixInv)) {
                message("Getting inverse from Cache...")
                return(matrixInv)
        }
        
        matrixList <- cacheMatrix$getMatrix()		#Get the matrix from cacheMatrix Object
        matrixInv <- solve(matrixList, ...)			#Perform the inverse calculation
        cacheMatrix$setInverse(matrixInv)			#Cache the result from the Matrix Inverse
        
        matrixInv
}
