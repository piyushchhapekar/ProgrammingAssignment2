## Functions below calculate inverse of a mtrix and caches it.
## If matrix does not change then it serves up inverse from the cache rather
## than recalculating each time

## makeCacheMatrix creates a list of function to 
## get and set matrix value
## get and set the matrix inverse
## this function does not calculate the inverse but rather just caches it

makeCacheMatrix <- function(x = matrix()) {

    invx <<- NULL

    set <- function(y)
    {
        x <<- y
        invx <<- NULL
    }

    get <- function() x

    setInverse <- function(inverse) invx <<- inverse

    getInverse <- function() invx

    list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}


## cacheSolve returns the inverse of x
## it "asks" makeCacheMatrix to get inverse from cache
## if exists in cache, returns it 
## otherwise calculates it and asks makeCacheMatrix to cache it before returning

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    invx <- x$getInverse()

    if(!is.null(invx))
    {
        message("getting inverse from cache")
        return(invx)
    }

    mat <- x$get()

    invx <- solve(mat)

    x$setInverse(invx)

    invx
}
