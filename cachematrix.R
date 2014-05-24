## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## initializing the inversMatrix to NULL
    inverseMatrix <- NULL
    ## This function allows re-initializing the cacheMatrix,  
    ## it is an optional function, not implementing it would make the
    ## cacheMatrix immutable
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    ## returns the matrix value,  notice closure is used so the matrix value is only 
    ## exposed through the "get" function.
    get <- function() x
    
    ## This function allows to set the cache of the calculated inverse matrix
    setInverseMatrix <- function(im) inverseMatrix <<- im
    
    ## This function allows obtaining the cached inverse Matrix
    getInverseMatrix <- function() inverseMatrix
    
    ## returning a list of the externally exposed functions
    list(set = set, get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## obtaining cached inverseMatrix 
    im <- x$getInverseMatrix()
    ## when inverseMatrix is cached use it
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    ## inverseMatrix is not calculate it
    data <- x$get()
    im <- solve(data, ...)
    x$setInverseMatrix(im)
    im    ## Return a matrix that is the inverse of 'x'
}

