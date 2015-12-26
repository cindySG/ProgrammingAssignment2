## rProg-035, assignement2: write functions to cache the results 
## of an inverser matrix

## makeCacheMatrix function creates a specical matrix object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        list( set = set, get = get,
              setInv = setInv,
              getInv = getInv)                
}

## cacheSolve function computes the inverse of the special matrix
## returned by makeCacheMatrix. If the inverse has already been 
## calculated, then the cachesolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
        ## if it is in cache, return the cached one
        inv <- x$getInv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        ## Return a matrix that is the inverse of 'x',
        ## use the generic function "ginv" instead of "solve". 
        ## "ginv" will work for both square and non-square matrix
        library(MASS)
        data <- x$get()
        #inv <- solve(data)
        inv <- ginv(data)
        x$setInv(inv)
        inv
}
