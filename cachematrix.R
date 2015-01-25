## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = matrix()) {
    m_inverse <- NULL
    # cache the matrix from the input variable "y", and clear the inverse product
    set <- function(y) {
        x <<- y
        m_inverse <<- NULL
    }
    get <- function() x
    set_inverse <- function(solve_result) m_inverse <<- solve_result
    get_inverse <- function() m_inverse
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)  
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
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
