## This script has two functions.
## -makeCacheMatrix: creates an special matrix wich can store its inverse matrix 
## for caching it
## -cacheSolve: computes the inverse of the special matrix and returns it

## The function below this comments, returns a list of functions:
## -set: set new data of the special matrix when the user requires it
## -get: returns the data of the special matrix when the user requires it
## -setInverse: set the data of the inverse of the special matrix and caches it
## -getInverse: returns the data of the inverse matrix

makeCacheMatrix <- function(X = matrix()) {
    IX <- NULL
    set <- function(Y){
        X <<- Y
        IX <<- NULL
    }
    get <- function() X
    setinverse <- function(inverse) IX <<- inverse
    getinverse <- function() IX
    list(set = set, 
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}

## This function computes the inverse of a matrix if it has not been computed 
## before, caches it and returns its inverse. If it has been computed before, 
## only shows a message and returns its cached inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    IX <- X$getinverse()
    if(!is.null(IX)) {
        message("getting cached data")
        return(IX)
    }
    data <- X$get()
    IX <- solve(data, ...)
    X$setinverse(IX)
    IX
}
