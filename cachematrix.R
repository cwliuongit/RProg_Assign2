# ------------------------------------------------------------------------ #
#
# 1. The function makeCacheMatrix creates an object, cacheMatrix, 
#       which consists of 4 functions:
#
#     1) cacheMatrix$get.Matrix() :
#          return a matrix that is cached in this object
#
#     2) cacheMatrix$get.inverse() :
#          retrun the inverse of the cached matrix
#
#     3) cacheMatrix.$set.Matrix(matrix_value) :
#          set the matrix cached in the object to matrix_value
#
#     4) cacheMatrx.$set.inverse(inverse_value) :
#          set the inverse of the cached matrix to inverse_value
#
# 2.  The function cacheSolve takes the object cacheMatrix, 
#       created by makeCacheMatrix, as the parameter, 
#         then computes the inverse of the matrix cached in cacheMatrix
#           using the following policy:
#
#       if (the inverse has been cached) {
#           simply return the cached inverse
#       } else {
#	         i) compute the inverse 
#           ii) cached the inverse to cacheMatrix
#          iii) return the computed inverse 
#       } 
# 
# ------------------------------------------------------------------------ #



## Function : makeCacgeMatrix(M)
## Parameter M : a numeric matrix to be cached along with its inverse
## Return: a list of functions : (get.Matrix, get.inverse, set.Matrix, set.inverse)               
makeCacheMatrix <- function(M = matrix()) {
	
	inverse <- NULL
	
	get.Matrix <- function() M
	
	get.inverse <- function() inverse
	
	set.Matrix <- function(M_value) {
		M <<- M_value
		inverse <<- NULL
	} # end of function set.Matrix
	
	set.inverse <- function(inverse_matrix) {
	    inverse <<- inverse_matrix	
	} # end of function set.inverse
	
	list(get.Matrix = get.Matrix,
		 get.inverse = get.inverse,
		 set.Matrix = set.Matrix,
		 set.inverse = set.inverse 	
	    ) # end of list
		
} # end of function makeCacheMatrix



## Function : cacheSolve
## Parameter x : an onject, cacheMatrix, created by the function makeCacheMatrix
## Return the inverse of the matrix cached in cacheMatrix
cacheSolve <- function(x, ...) {
	
	inverse <- x$get.inverse()
	
	if (is.null(inverse)) {
		M <- x$get.Matrix()
		inverse <- solve(M)
		x$set.inverse(inverse)
		return(inverse)
	} else {
		message("simply return cached inverse")
		return(inverse)
	} # end if
	
} # end of function cacheSolve
