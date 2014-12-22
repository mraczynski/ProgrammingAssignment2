## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    #reset inverse matrix
    inv <- NULL
    
    # define CachedMatrix methods
    # to set matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL 
    }
    # to get matrix
    get <- function() x
    #set inverse matrix (search parent environment)
    setinv <- function(invx) inv <<- invx
    #get inverse matrix
    getinv <- function() inv
    # return structured 
    list(set = set, get = get, setinv = setinv, getinv = getinv)
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    # try to get cached inversed matrix
    inv <- x$getinv()
    
    # if cached inverse found display message 
    # and return it without calculating
    if(!is.null(inv)) {
        message("found cached inversed matrix")
        return(inv)
    }
    # if not found get matrix
    invx <- x$get()
    # calculate inverse
    inv <- solve(invx, ...)
    # set cache to calculated value
    x$setinv(inv)
    #return inverse matrix
    x$getinv()
}
