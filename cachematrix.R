## This R script contains two functions to caching the inverse of a matrix
## rather than compute it repeatedly

## makeCacheMatrix is used to create a "special" matrix which is used to set,
## get, setinverse and getinverse for the matrix given as argument

## example:
##   temp <- makeCacheMatrix(matrix1) with matrix1 as a inversible matrix will give
##   to temp a list of functions called set, get, setinverse and getinverse

makeCacheMatrix <- function(x = matrix()) {
    # clean the cache when you create the object
    m <- NULL
    # create a inner function called set
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    #create a inner function called get
    get <- function() x
    #create a inner function called setinverse
    setinverse <- function(solve) m <<- solve
    #create a inner function called getinverse
    getinverse <- function() m
    #return the list of functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve solve the inverse of the matrix but if the inverse is already
## calculated return the cached version of the solution

cacheSolve <- function(x, ...) {
    # get the inverse from the especial matrix
    m <- x$getinverse()
    # if the value exists return the cached version
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # get the data in case that the cached version was not returned
    data <- x$get()
    # create the invesr matrix
    m <- solve(data, ...)
    # set it to the special matrix and stored in the cache of the special matrix
    x$setinverse(m)
    ## Return a matrix that is the inverse of 'x'
    m
}
