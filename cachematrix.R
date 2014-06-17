## functions to implement a cached inverse square matrix.
##
## 2 steps:  first create the "special matrix", which is actually
## a list of functions that set/get the the matrix and the solved
## value, then actually solve the matrix.
##
## command line usage is as follows, given a sample matrix to solve:
##      > m <- matrix(c(2,2,3,2), nrow=2, ncol=2)
##      > my_mat <- makeCacheMatrix(m)
##      > cacheSolve(my_mat)
## #################################################################

## makeCacheMatrix argument is an invertible square matrix. if no
## argument is provided, the function creates an empty matrix
## (probably not what you want).
##
## function returns a list of setters/getters for the cached object.
##
## NOTE the 'set' function of the object is not actually called
## internally. it can be called from the returned object to reuse
## it with another matrix, instead of creating a new cached object:
##      > my_mat$set(matrix(c(4,3,3,2), nrow=2, ncol=2))
##      > cacheSolve(my_mat)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) m <<- solve
        getSolve <- function() m
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## cacheSolve returns a matrix that is the inverse of its
## argument 'x', using the solve() function.  it will cache
## the value of solve(), and return it upon subsequent calls
## instead of calculating it each time.
##
## argument 'x' is a "special" matrix created by the makeCacheMatrix
## function above.  "..." args are anything additional you want to
## pass to the solve() function.

cacheSolve <- function(x, ...) {
        ## 
        m <- x$getSolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setSolve(m)
        m
}
