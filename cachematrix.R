## This function will cache the inverse of a matrix to save computation time
## due to repetition of the inversion operation.

## The function makeCacheMatrix() will create a list of functions which will
## set the matrix, retrieve the matrix, calculate the inverse of the retrieved
## matrix and retrieve the inversed matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL ## initiates inverse holder as a null variable
        set <- function(y){ ## creates the set function for a global variable
                x <<- y
                inv <<- NULL
        }
        get <- function() x ## creates the get function to retrieve
        setinv <- function(solve) inv <<- solve ## creates the function to
                                                ## generate the inverse
        getinv <- function() inv ## creates the function to retrieve the inverse
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The function cacheSolve() will calculate the inverse of the matrix which was 
## created and cached using makeCacheMatrix.

cacheSolve <- function(x, ...) {
        inv <- x$getinv() ## retrieves the inverse of the matrix
        if(!is.null(inv)){ ## checks to make sure that the inverse exists
                message("getting cached data")
                return(inv)
        }
        data <- x$get() ## sets the original matrix to 'data' variable
        inv <- solve(data,...) ## calculates the inverse of the original matrix
        x$setinv(inv) ## sets the inverse of the matrix globally
        inv
}
