
## The following function creates an object with methods for working with the underlying matrices       
##
##      set             - sets    the original matrix and clears the previous inverse
##      get             - returns the current matrix
##      set_inv_matrix  - stores  the calculated inverse of the original matrix
##      get_inv_matrix  - returns the calculated inverse of the original matrix



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


## Following function calculates the inverse matrix if the result from previous calculations is undefined 
## otherwise itr returns already stored result of the last calculation.   

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
