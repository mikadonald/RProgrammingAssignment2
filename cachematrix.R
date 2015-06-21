## Below are two functions that are used to create a special object that stores a numeric vector and cache's its inversed matrix.

## The first function, makeCacheMatrix creates a special "vector", which is really a list
## containing a function to 
## 1. Set the value of the matrix
## 2. get the value of the matrix
## 3. Set the value of the inversed matrix
## 4. Get the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The following function generates the inversed matrix of the matrix created with the above function. However, it first checks to see if the inversed matrix has been generated. If so, it would return the result from the cache and skips the computation. Otherwise, it would return the inverse of the matrix and set the value in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
