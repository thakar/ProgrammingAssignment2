## The following two functions exploit the lexical scoping rules of R to 
## create a "lazy" version of the solve function to get the inverse of a
## matrix.  This version, cacheSolve, first looks for a cached version of the
## inverse, and if it exists, just returns that. The cached version exists
## if the first function, makeCacheMatrix, has been called beforehand to
## constuct a list where the solved version (inverse) can be stored. The
## subsequent calls to cacheSolve use the cached solution.

## makeCacheMatrix: This function creates a list that contains 4 fucntions 
## that q) set the matrix, b) get the matrix, c) set the matrix inverse, and
## get the matrix inverse.  These functions, when invoked from the second 
## function cacheSolve use the lexical scoping rules to use cached values
## of the matrix and its inverse when available.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL			# reset the local copy of solution 
        set <- function(y) {	# function to set the matrix
                x <<- y
                s <<- NULL	# reset the copy in the environment
        }
        get <- function() x	# function to get the matrix
        setSolve <- function(solve) s <<- solve	# function to set solution
        getSolve <- function() s			# function to get solution
        list(set = set, get = get,			# save them all in the list
             setSolve = setSolve,
             getSolve = getSolve)
}


## cacheSolve: When given the list created by makeCacheMatrix, this function
## looks for a solved solution (inverse) cached in that list, and if it is
## found, just returns it and skips the second half that calls the solve 
## function to return its inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getSolve()	# check to see if list already has solution
        if(!is.null(s)) {	# if a solution is cached, just return that
                message("getting cached data")
                return(s)
        }
        data <- x$get()		# get the original matrix from the list
        s <- solve(data, ...)	# call solve function to obtain its inverse
        x$setSolve(s)		# save the solution in the list
        s				# return the solution
}
