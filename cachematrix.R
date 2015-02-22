## Caching the Inverse of a Matrix


#This function will store its inverse as cache.

makeCacheMatrix <- function(x = matrix()) 
{
    m <- NULL

    # Set the value of the matrix
    set <- function(y) 
    {
        x <<- y
        m <<- NULL
    }
    
    # Get the value of the matrix
    get <- function() x
    
    # Set the value of the inverse of the matrix
    setsolve <- function(solve) m <<- solve
    
    # Get the value of the inverse of the matrix
    getsolve <- function() m
    
    # Return
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

# cacheSolve fuction will calculate the inverse of the matrix x.
# If the inverse has already been calculated, then cacheSolve will retrieve
# the solution from the cache.

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
