## These functions can cache inverse of the matrix that have been entered.
## So when one wants to calculate the inverse of the matrix, it will return the cached data
## if the matrix has been entered before; otherwise it will calculate the inverse and return.

## This function calculates and caches the inverse of the matrix, returns a list.

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y) {
x <<- y
inv <<- NULL
}
get <- function() x
setinverse <- function(solve) inv <<- solve
getinverse <- function() inv
list(set = set, get = get,
setinverse = setinverse,
getinverse = getinverse)
}


## This function calculates the inverse of the matrix if it hasn't been cached before; otherwise retrive the cached data. Returns the inverse matrix.

cacheSolve <- function(x, ...) {
inv <- x$getinverse()
if(!is.null(inv)) {
message("getting cached data")
return(inv)
}
data <- x$get()
inv <- solve(data, ...)
x$setinverse(inv)
inv        ## Return a matrix that is the inverse of 'x'
}
