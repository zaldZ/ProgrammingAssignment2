## makeCacheMatrix creates a list of functions that can cache the inverse
## of a given matrix x
## cacheSolve evaluates the inverse of the matrix stored by the previous
## function makeCacheMatrix and caches it


## Given a matrix x, makeCacheMatrix() creates a list containing four functions:
## set(), get(), setinverse() and getinverse()
makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        
        ## set() takes a matrix as input and sets it as the cached matrix
        set <- function(y){
                x <<- y
                invMatrix <<- NULL
        }
        
        ## get() returns the cached matrix
        get <- function() x
        
        ## setinverse() takes a matrix as input and sets it as the cached
        ## inverted matrix
        setinverse <- function(inverse) invMatrix <<- inverse
        
        ## getinverse() returns the cached inverted matrix
        getinverse <- function() invMatrix
        
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Given a cached matrix, cacheSolve() evaluates the inverse of that same matrix
## if it has not yet been evaluated.
cacheSolve <- function(x, ...) {
        invMatrix <- x$getinverse()
        if(!is.null(invMatrix)){
                message("getting cached data")
                return(invMatrix)
        }
        data <- x$get()
        invMatrix <- solve(data)
        x$setinverse(invMatrix)
        invMatrix
        
}
