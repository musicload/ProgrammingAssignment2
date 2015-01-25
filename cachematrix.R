## This R script includes two functions: makeCacheMatrix and cacheSolve.
# The functions use the advantages of lexical scoping in R to initialize
# a list object containing a matrix, to calculate the inverse of a matrix,
# and to cache the inverted matrix for further re-use.

## The function "makeCacheMatrix" creates a list object containing 4 functions:
# set() is used to cache a matrix, and to clear the variable holding the inverse product of a matrix
# get() is used to display the matrix stored in the object
# set_inverse() is used to cache the inverse product of a matrix
# get_inverse() is used to display the inverse product of a matrix

## Note: the set_inverse() function should never be called directly for the 
# object created with makeCacheMatrix function. 
# It works properly only if called by another function (e.g. cacheSolve )
# which implements the logic for matrix inversion.

makeCacheMatrix <- function(x = matrix()) {
    m_inverse <- NULL
    # cache the matrix from the input variable "y", and clear the inverse product
    set <- function(y) {
        x <<- y
        m_inverse <<- NULL
    }
    get <- function() x
    # cache the inverse matrix
    # (inversion itself is done in the calling function "cacheSolve"!)
    set_inverse <- function(solve_result) m_inverse <<- solve_result
    get_inverse <- function() m_inverse
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)  
}

## The function "cacheSolve" calculates the inverse matrix, or 
# retrieves the cached value if it has been already calculated.
# The input variable "x" is expected to be a list, with functions 
# as list elements (compatible to the "makeCacheMatrix" function output).

# The inversion is done with the standard R function solve(). 
# The additional variables ("..." argument) will be passed to 
# solve() if those variables were specified. 
# This helps to further customize the logic.

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    # (either by retrieving the cached value, or by performing the calculation)    
    m_inverse <- x$get_inverse()
    if(!is.null(m_inverse)) {
        message("getting cached data for inverted matrix")
        return(m_inverse)
    }
    data <- x$get()
    m_inverse <- solve(data, ...)
    x$set_inverse(m_inverse)
    m_inverse
}
