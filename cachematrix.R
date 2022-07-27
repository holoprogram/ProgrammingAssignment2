## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL  # Cache the values of the inverse matrix
        set <- function(y){
                x <<- y
                m <<- NULL
                # use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 
        }
        get <- function() x
        
        setinverse <- function(inv) m <<- inv
        getinverse <-  function() m
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        # if the inverse has already been calculated
        if(!is.null(m)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(m)
        }
        
        # otherwise, calculates the inverse 
        data <- x$get()
        m <- solve(data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinverse(m)
        
        return(m)
        
}
