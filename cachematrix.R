## This function will store all the functions used to store an invertible matrix
## and it's cached inverse
## As said before, we assume all matrices passed to this function are invertible
makeCacheMatrix <- function(x = matrix()) {
        ### @x: an invertible matrix
        ### @i: is the inverse matrix of x, it's value is NULL to start
        i <- NULL
        ### set is a function that will set it's argument y equal to x
        ### and it will set i equal to NULL because how can we know the inverse
        ### immediately?
        set <- function(y) {
          x <<- y
          i <<- NULL
        }
        ### get is a function that will return the matrix x
        get <- function() x
        ### setinverse will set the inverse of the matrix x to matrix i
        setinverse <- function(solved) i <<- solved
        ### getinverse is a function that will return the matrix i
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function calculates the inverse of a matrix if it is not already found
## in the cache
cacheSolve <- function(x, ...) {
        ### First check to see if "i" is not equal to null
        i <- x$getinverse()
        if(!is.null(i)) {
          ### If it is not, return the cached inverse
          message("getting cached data")
          return(i)
        }
        ### Otherwise, calculate the inverse, and set the solved value equal to i
        matrixdata <- x$get()
        i <- solve(matrixdata, ...)
        x$setinverse(i)
        ### Return a matrix that is the inverse of 'x'
        i
}
