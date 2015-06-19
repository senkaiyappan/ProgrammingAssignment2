## makeCacheMatrix function creates a special "matrix" 
## object that can cache its inverse
## Initialize Special Matrix and Cache Matrix
## Get and Set Special matrix
## Get and Set Inverse of Special Matrix

makeCacheMatrix <- function(x = matrix()) {
    # Initialize the Inverse Cache to NULL
    invMatrix <- NULL
    # Set function to assign and initialize matrix
    set <- function(y) {
        x <<- y
        invMatrix <<- NULL
    }
    
    #get function to return the matrix
    get <- function() x
    
    #set function to set the inverse of matrix to cache
    setinverse <- function(inverseM) invMatrix <<- inverseM
    
    #get function to return the cached Inverse
    getinverse <- function() invMatrix
    
    #Returns the list of named functions for makeCacheMatrix function
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
    
}


## cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    invMatrix <- x$getinverse()
    
    # Check if the cache matrix exists
    if(!is.null(invMatrix)) {
        message("Retrieving Cached Matrix...")
    }
    else{
        message("Calculating Matrix Inverse...")
        # Get the Originial Matrix
        specialMatrix <- x$get()
        # Calculate Inverse of the Special Matrix
        invMatrix <- solve(specialMatrix)
        # Set the inverse of Special Matrix to Cache
        x$setinverse(invMatrix)
    }
    ## Returns the inverse of Matrix 'x'
    return(invMatrix)
    
}

## Execution sample is given below:
##
## > m = makeCacheMatrix(rbind(c(2,3),c(3,2)))
## > cacheSolve(m)
## Calculating Matrix Inverse...
## [,1] [,2]
## [1,] -0.4  0.6
## [2,]  0.6 -0.4
## > cacheSolve(m)
## Retrieving Cached Matrix...
## [,1] [,2]
## [1,] -0.4  0.6
## [2,]  0.6 -0.4
## > 
