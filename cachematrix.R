## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        
        ## This variable will store the cached inverse
        inv <- NULL
        
        ## Function to set a new matrix
        ## When the matrix changes, the cached inverse is cleared
        set <- function(y) {
                x <<- y          ## Assign new matrix to x
                inv <<- NULL     ## Reset cached inverse
        }
        
        ## Function to get the current matrix
        get <- function() {
                x
        }
        
        ## Function to store the inverse of the matrix
        setinverse <- function(inverse) {
                inv <<- inverse
        }
        
        ## Function to return the cached inverse (if it exists)
        getinverse <- function() {
                inv
        }
        
        ## Return a list of functions to interact with the special matrix
        list(
                set = set,
                get = get,
                setinverse = setinverse,
                getinverse = getinverse
        )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated, it retrieves it from the cache
cacheSolve <- function(x, ...) {
        
        ## Try to get the cached inverse from the special matrix object
        inv <- x$getinverse()
        
        ## If the inverse is already cached, return it
        if (!is.null(inv)) {
                message("getting cached inverse")  ## Inform the user that cached data is used
                return(inv)                         ## Return the cached inverse and exit the function
        }
        
        ## If the inverse is not cached, compute it
        data <- x$get()                       ## Get the matrix from the object
        inv <- solve(data, ...)                ## Compute the inverse of the matrix
        x$setinverse(inv)                      ## Cache the computed inverse for future use
        
        inv                                    ## Return the newly computed inverse
}