## Caching the Inverse of a Matrix

makeCacheMatrix <- function(x = matrix()) 
{
    m <- NULL

    # set the value of the matrix
    set <- function(y) 
    {
        x <<- y
        m <<- NULL
    }
    
    # get the value of the matrix
    get <- function() x
    
    # set the value of the inverse of the matrix
    setsolve <- function(solve) m <<- solve
    
    # get the value of the inverse of the matrix
    getsolve <- function() m
    
    # return
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


cacheSolve <- function(x, ...) 
{
    
    # Return a matrix that is the inverse of 'x'
    m <- x$getsolve() # retrieve from cache
    if(!is.null(m)) 
    { 
        message("getting cached data")
        return(m)
    }
    
    data <- x$get() 
    m <- solve(data, ...) 
    x$setsolve(m) 
    m    
}
