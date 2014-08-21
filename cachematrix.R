## makeCacheMatrix() basically accept a matrix as input 
## and returns a List which contains four functions.
## The List of functions from makeCacheMatrix() will be pass 
## as input into cacheSolve(). cacheSolve will solve for the 
## inverse matrix value ## and the inverse matrix will get 
## stored in variable inverse_m_cache. 

makeCacheMatrix <- function(input_matrix = matrix()) {
	
	## set the inverse matrix cache to be NULL 
	## (empty) at the start.
	#######################	
	inverse_m_cache <- NULL
	
	## this redundant function will set the matrix 
	## to be the specified input value. It will also 
	## reset the inverse matrix cache to be NULL (empty).
	###########################
	set_matrix <- function(y) {
		input_matrix <<- y
		inverse_m_cache <<- NULL
      }

	## this function returns the user input matrix 
	## from makeCacheMatrix. 
	##########################
	get_matrix <- function() {
		input_matrix
	}
	
	## this function will store the inverse matrix 
	## calculated from cacheSolve().
	################################################
	set_inverse_matrix <- function(inverse_matrix) {
		inverse_m_cache <<- inverse_matrix
	}
	
	## this function return the value of inverse matrix. 
	##################################
	get_inverse_matrix <- function() {
		inverse_m_cache
	}
	
	## this is a list that stores all the four 
	## functions mentioned above. this list will
	## be returned as output from makeCacheMatrix function.
	######################################################
	list(set_matrix = set_matrix, get_matrix = get_matrix, 
		set_inverse_matrix = set_inverse_matrix, 
		get_inverse_matrix = get_inverse_matrix)
}


## cacheSolve() gets the input matrix from makeCacheMatrix() 
## and solves for the inverse matrix and then passes it back 
## to get stored in variable inverse_m_cache.
## If the inverse matrix data has already been stored 
## previously in the variable inverse_m_cache, then cacheSolve 
## will not perform the solving procedure again, as it will 
## recall straight from the cache (inverse_m_cache).  

cacheSolve <- function(x, ...) {

	## get inverse matrix from makeCacheMatrix(). 
	########################################	
	inverse_matrix <- x$get_inverse_matrix()

	## check to see if the inverse_matrix has already exist.
	##############################
	if(!is.null(inverse_matrix)) {
		message("getting cached of inverse matrix")
		return(inverse_matrix)
	}
	
	## get matrix data from makeCacheMatrix().
	#########################################
	matrix_data <- x$get_matrix()

	## solve the inverse of the matrix. 
	########################################
	inverse_matrix <- solve(matrix_data, ...)
	
	## store the inverse matrix data into makeCacheMatrix().
	####################################
	x$set_inverse_matrix(inverse_matrix)
	
	## returns the inverse matrix value.
	##############
	inverse_matrix
}
