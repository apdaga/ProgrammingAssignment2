## Put comments here that give an overall description of what your
## functions do


## function to cache the matrix (input should be of Matrix class)

makeCacheMatrix <- function(x = matrix()) {     

        inv <- NULL                             ## initialize inv to NULL
        set <- function(y) {
                x <<- y                         ## create a special object "x" for input matrix
                inv <<- NULL                    ## create a special object "inv" for output matrix
        }
        get <- function() x                             ## function to get the value of input matrix "x"
        setinv <- function(inverse) inv <<- inverse     ## function to set the value of input matrix "x"
        getinv <- function() inv                        ## function to get the value of output matrix "inv" (inverse of matrix x)
        
        ## creating list of functions in order to use functions with $ symbols
        list(set = set, 
             get = get,
             setinv = setinv,
             getinv = getinv)
}


## function to calculate the inverse of the input matrix "x" and return the output as "inv"

cacheSolve <- function(x, ...) {        
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()                               ## get the value of matrix "inv" by calling "getinv" function
        if(!is.null(inv)) {                             ## check if the variable "inv" is NULL
                message("getting cached data")          ## print message on console
                return(inv)                             ## if variable "inv"  has some value other than NULL in the cache, return the same.
        }
        data <- x$get()                                 ## get the input matrix "x" by calling "get" function
        inv <- solve(data, ...)                         ## calculate inverse of the matrix by "solve" function
        x$setinv(inv)                                   ## set the value of inverse of the input matrix by "setinv" function
        inv                                             ## return the calculated inverse of the matrix.
}
