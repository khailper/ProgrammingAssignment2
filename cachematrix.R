## These two fuctions combine to allow us to find the inverse of a matrix and store that inverse so we don't have to 
## recalculate it every time; we can just retrieve it.

## This function finds the inverse of the matrix and stores it in the cache.

makeCacheMatrix <- function(x = matrix()) {
       inv <- NULL
       ## Used "inv" instead of conciser "i" (like "m" in the example) to avoid confusion since "i" is 
       ##used for iteration.
       ## Code creates variable for inverse but doesn't give it a value in order to avoid falsely
       ##returning an inverse if input is non-invertable
        set <- function(y) {
                x <<- y
                inv <<- NULL
                ##Allows us to change value of x later, but resets inv since new value can't be assumed to
                ##have same inverse.
                ##"<<-" ensures changes happen in makeCacheMatrix environment, not just in
}
get <- function() x
## this lets cacheSolve retreive x if the inverse doesn't exist
setinverse <- function(solve) inv <<- solve
##this finds the inverse and seaves it in the cache
getinverse <- function() inv
## this lets cacheSolve retrive the inverse if it exists
list(set = set, get = get,
     setmean = setinverse,
     getinverse = getinverse)
#this makes sure our matrix has the functions above so we can use them
}


## This function checks to see if we previously found the inverse of x so it can avoid duplicating work if we already 
##did.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        ##we fetch the inverse (or NULL) from makeCacheMatrix
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ##if the inverse already has been found, we return it save the bother of recalculating it
        ##if inv == NULL, then either the inverse hasn't been found or the matrix has changed without recalculating inv
        data <- x$get()
        ##we retrieve the matrix
        inv <- solve(data, ...)
        ##and find its inverse
        x$setinverse(inv)
        #we assign the inverse to inv in case we want it again
        inv
        #lastly, we return the found inverse
}
