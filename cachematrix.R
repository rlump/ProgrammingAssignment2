## The folowing functions cache a matrix and it's inverse. The inverse is
## calculated via r's built in solve method.

## makeCacheMatrix - object that caches a matrix and it's inverse
##  args - a matrix (defaults to empty matrix)
##  return - list of functions to manipulate the cached matrix and it's inverse
##           get - get the cached matrix
##           set - set the cached matrix
##           getinverse - get the cached matrix's inverse
##           setinverse - set the cached matrix's inverse
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
                x <<- y
                m <<- NULL
        }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve - gets (or sets if NULL) the cached inverse 
##              matrix for a matrix cached by makeCacheMatrix
##  args - makeCacheMatrix object
##  return - inverse of matrix cached in makeCacheMatrix object
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
