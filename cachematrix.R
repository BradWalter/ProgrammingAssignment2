## matrix inversion tools that avoid recalculating the inverse of a matrix
## if it has already been calculated using lexical scoping
## note these tools are really just an exercise in lexical scoping.To
## be really usefull they need to be able to retain the names of all matrices
## for which inversion had been attempted and test to see if the new matix
## was included.  

## MakeCacheMatrix creates an input list of utility functions 
## for cacheSolve run before 'cacheSolve'  and assign to <listname>
## 'set' supplies matrix to be inverted and'tried' <- FALSE in 
##  makeCacheMatrix makeCacheMatrix
## 'get' function returns the matrix being inverted
## 'getstat' returns 'tried' indicating if iverting has been attempted
## 'setstat' sets 'tried in makeCacheMatrix to 'TRUE' if inverting attempted
## 'setinv' sets inv in makeCacheMatrix environment to inverse if calculated
## 'getinv' gets inv from makeCacheMatrix environment

makeCacheMatrix <- function(x = matrix()) {
    ## inverse may not exist. Flag 'tried' set to TRUE if 
    ## inversion is attempted
    tried <- FALSE
    inv <- matrix(NA, 1, 1)
    set <- function (y) {
        x <<- y
        tried <<- FALSE
        inv <<- matrix(NA, 1, 1)
    }
    get <- function() x
    getstat <- function() tried
    setstat <- function(stat) tried <<- stat
    setinv <- function(invse) inv <<- invse
    getinv <- function() inv
    list(set = set, setstat = setstat, get = get, getstat = getstat, 
         setinv = setinv, getinv = getinv)
}



## Tests 'tried' see if inverting was attempted for matrix specified by 'set'.
## sets 'tried' in makeCacheMatrix environment to TRUE when inveteing attempted.
## calculates and sets value of 'inv'in makeCacheMatrix and returns the inverse,
## if possible. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    tried <- x$getstat()
    inv <- x$getinv()
    if(tried == TRUE) {
        message("getting chached data")
        return(inv)
    }
    mat <- x$get()
    tried <- TRUE
    x$setstat(tried)
    inv <- solve(mat, ...)
    x$setinv(inv)
    inv
}

