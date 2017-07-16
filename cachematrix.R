
## The following function creates an object with methods
## for working the underlying matrix

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        set_inv_matrix <- function(inv_matrix) im <<- inv_matrix
        get_inv_matrix <- function() im
        list(set = set, get = get,
             set_inv_matrix = set_inv_matrix,
             get_inv_matrix = get_inv_matrix)
}


## Following function calculates the inverse matrix if the result from previous calculationsis undefined. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                im <- x$get_inv_matrix()
                if(!is.null(im)) {
                        message("getting cached data")
                        return(im)
                }
                im <- solve(x$get(), ...)
                x$set_inv_matrix(im)
                im
}
