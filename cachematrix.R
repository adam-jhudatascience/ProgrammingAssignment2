## The functions below implement a special object called a 
## cache matrix. It is a wrapper for a regular invertible
## matrix that has the ability to store a cached value of
## the inverse of the matrix if it has been previously
## calculated, in order to avoid repeated (and potentially
## costly) computations of the inverse.

## This function is a constructor for a cache matrix. It takes
## a numeric matrix x (assumed to be invertible) as input and
## returns a list (set, get, setinverse, getinverse) where:
## 1. set is a function that sets the value of the internal matrix
##      within the cache matrix to x;
## 2. get is a function that returns the value of the internal matrix.
## 3. setinverse is a function that takes an argument inv and
##      sets the inverse of the internal matrix to inv;
## 4. getinverse returns the most recently computed value of the inverse
##      of the internal matrix obtained by using cacheSolve() (or NULL 
##      if the inverse has never been computed).

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y 
                i <<- NULL
        }
        get <- function() {
                x
        }
        setinverse <- function(inv) {
                i <<- inv
        }
        getinverse <- function() {
                i
        }
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function takes an argument x, which is a cache matrix
## assumed to have been created via the makeCacheMatrix() function
## above. It then returns the cached value of the inverse of x if 
## it has previously been computed; otherwise it computes the inverse
## using solve() and caches the result before returning it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
