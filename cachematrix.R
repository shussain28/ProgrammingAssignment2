## first function makecacheMatrix creates a special matrix object that can cache its inverse
## second function cacheSolve computes the inverse of the special matrix returned by makeCacheMatric function
## If the inverse has already been calculated, then the cacheSolve fetches the inverse from the cache.

## makeCacheMatrix creates a special "matrix", which is a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x=matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## Returns a matric that is inverse of 'x'
## it first checks to see if the inverse is already calculated. If it is, then it gets the inverse from 
## the cache, otherwise it calculates the inverse of the matrix and sets the value of the inverse
## in the cache via the getinverse function call.

cacheSolve <- function(x, ...) {
        t <- x$getinverse()
        if(!is.null(t))
                {  message("getting cached data")
                   return(t)
                    }
       
        c <- solve(x$get())
        x$setinverse(c)      
        c
}
