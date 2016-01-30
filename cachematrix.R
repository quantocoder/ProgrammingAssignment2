## makeCacheMatrix
## The function creates a special "matrix", which is a list containing a function to
## set the value of the vector
## get the value of the vector
## set the value of the mean
## get the value of the mean

makeCacheMatrix <- function(m = matrix()) {
    inv <- NULL
    set <- function(y) {
        m <<- y
        inv <<- NULL
    }
    get <- function() { m }
    setinv <- function(inverse) { inv <<- inverse }
    getinv <- function() { inv }
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## The following function calculates the mean of the special "matrix" created 
## with the above function. However, it first checks to see if the inverse has 
## already been calculated. If so, it gets the inverse from the cache and skips
## the computation. Otherwise, it calculates the inverse of the data and sets 
## the value of the inverse in the cache via the setinve function.

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- m$getinv()
        if(!is.null(inv)) {
            message("getting cached data")
            return (inv)
        }
        data <- m$get()
        inv <-  solve(data, ...)
        m$setinv(inv)
        inv
}
