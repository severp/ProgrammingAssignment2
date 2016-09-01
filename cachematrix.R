## The cachematrix.R file contains two functions: makeCacheMatrix and cachesolve.
## The first one stores a matrix and its inverse, the second computes the inverse
## when it is first invoked and caches it. If the inverse is already stored it
## retrieves it from cache.


## makeCacheMatrix creates an object that stores four functions: set, get, setinv,
## getinv. It also stores the matrix to be inverted and the inverse after being
## calculated.

makeCacheMatrix <- function(x = matrix()) {     ## The argument is set as a matrix
        inv <- NULL             ## The inverse matrix is initialized NULL
        set <- function(y){     ## set function sets matrix x to its argument y 
                x <<- y         ## and initializes inverse to NULL when called
                inv <<- NULL    ## from  the function cachesolve.
        }
        get <- function()x      ## retrieves matrix x
        setinv <- function(inverse) inv <<- inverse ## sets the inverse matrix
        getinv <- function()inv         ## retrieves the inverse matrix
        list(set = set, get = get,     ## makes a list of the above four functions
             setinv = setinv, getinv = getinv)
}


## This function takes as argument x the object provided by makeCacheMatrix and
## returns the inverse of the matrix either by computing it or, if it was
## already computed, from cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()       ## retrieves the inverse matrix if calculated yet
                                ## or null.
        if(!is.null(inv)){      ## if not null  
                message("getting cached data")          ## prints the message and
                return(inv)     ## the inverse matrix from cache
        }
        data <- x$get()         ## if inv is null retrieves the matrix
        inv <- solve(data, ...) ## computes the inverse of the matrix
        x$setinv(inv)           ## stores the inverse matrix
        inv                     ## returns the calculated inverse matrix
        ## Return a matrix that is the inverse of 'x'
}
