## ## makeCacheMatrix contains a matrix and cache its inverse version
## return: a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
## this list is used as the input to cacheSolve()
 
makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    get <- function() x
    setinverse<- function(inverse) inv_x <<-inverse
    getinverse <- function() inv_x
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
 
## cacheSolve computes the composite data structure created by makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## cacheSolve computes the composite data structure created by makeCacheMatrix
    ## once the inverse of matrix is computed, it will automatically 
    ## cache it into the struture of makeCacheMatrix
    inv_x <- x$getinverse()
    if (!is.null(inv_x)) {
        message("getting cached inverse matrix")
        return(inv_x)
    } else {
        inv_x <- solve(x$get())
        x$setinverse(inv_x)
        return(inv_x)
    }
}