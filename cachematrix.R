## The first function, `makeVector` creates a list containing a function to:
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse
# 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL
            set <- function(y) {
                    x <<- y
                    inv <<- NULL
            }
            get <- function() x
            setsolve <- function(solve) inv <<- solve
            getsolve <- function() inv
            list(set = set, get = get,
            setsolve = setsolve,
            getsolve= getsolve)
}

# The following function calculates the inverse of the matrix created with the previous function. 
# It first checks if the inverse has already been calculated. If so, it `get`s the inverse from the
# cache. If not, it compute the inverse using the data and stores it in the cache.

cacheSolve <- function(x, ...) {
            inv <- x$getsolve()
            if(!is.null(inv)) {
                    message("getting cached data")
                    return(inv)
            }
            data <- x$get()
            inv <- solve(data, ...)
             x$setsolve(inv)
            inv
}

