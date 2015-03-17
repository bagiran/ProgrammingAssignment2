#############################################################################################################
#
#  This is programming assignment 2 for course "R Programming" https://www.coursera.org/course/rprog
#
#  This assignment shall demonstrate R language skills in problem that requires resource-intensive 
#  computation with result which can be saved for future use. As an example of such problem used 
#  a problem of inverse matrix computing.
#
#  Example of using:  
#  >  m <- makeCacheMatrix(matrix(rnorm(16),4,4))
#  >  cacheSolve(m)
#
#  Code written by Nadezda Strikovskaya
#  GitHub account: https://github.com/bagiran/
#
#############################################################################################################


# makeCacheMatrix() function creates a special object stores the matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        # initialize cache as empty object
        s <- NULL
        
        # original matrix accessors
        set <- function(y) {
                x <<- y
                # reset cache for new matrix 
                s <<- NULL
        }
        
        get <- function() 
                x
        
        # cached inverse matrix accessors
        #   I think that setInverseValue/getInverseValue will be better 
        #   but it's not in line with name of cacheSolve() function
        setSolve <- function(solve) 
                s <<- solve
        
        getSolve <- function() 
                s
        
        list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)        
}

# cacheSolve() function computes the inverse of the x object created by makeCacheMatrix() function call
# The second call of cacheSolve() function for the same matrix should use cached value.

cacheSolve <- function(x, ...) {

        # get cached value
        s <- x$getSolve()
        
        # OK, have cache
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        
        # no cached data, calculate inverse matrix 
        s <- try(solve(x$get(), , ...))
        
        # oops, non-square or singular matrix - according course assignment it's impossible so it's fatal error
        if (class(s) != "matrix")
                stop("inverse matrix for given object does not exist")
        
        x$setSolve(s)
        s
}
