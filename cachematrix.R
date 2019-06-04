## Creates a matrix obj that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        ## initialise inverse
        a <- NULL
        ## set the matrix
        set <- function(y) {
                x <<- y
                a <<- NULL
        }
        ## get the matrix
        get <- function() x
        ## set inverse of mat
        set_inverse <- function(inverse) a <<- inverse
        get_inverse <- function() a
        ## return list of methods
        list(set = set,get = get,set_inverse = set_inverse,get_inverse = get_inverse)
        }

## find inverse of mat returned by makeCacheMatrix 
## if already calculated then get from cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        a <- x$get_inverse()
        ## return inverse if already calculated
        if (!is.null(a)) {
                message("getting cached data")
                return(a)
        }
        ## Get the matrix 
        data <- x$get()
        ## Calculate the inverse using matrix multiplication
        a <- solve(data, ...)
        ## Set the inverse 
        x$set_inverse(a)
        ## return matrix
        a
}
