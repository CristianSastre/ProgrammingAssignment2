## These functions allow you to do the inversion of a matrix efficiently,
## by caching the inverse of a matrix and then using it if necessary. 

## makeCacheMatrix create an object that can store an inverse value of matrix

makeCacheMatrix <- function(x = matrix()) {
    
    inv_matrix <- NULL ## Init inv_matrix
    
    set <- function(matrix){ ## Method to set the matrix
        m <<- matrix
        inv_matrix <<- NULL
    }
    
    get <- function() {m} ## Method the get the matrix

    ## Method to set the inverse of the matrix    
    setInverse <- function(inverse) {inv_matrix <<- inverse}
    
    ## Method to get the inverse of the matrix
    getInverse <- function() {inv_matrix}
    
    ## list of methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Computes the inverse of the matrix if it hasn't been computed, 
## otherwise it seeks to retrieve the inverse from the cache 

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    inv_matrix <- x$getInverse()
    
    ## check if the set has already been done 
    if(!is.null(inv_matrix)){
        message("getting cached data")
        return(inv_matrix)
    }
    
    ## get the matrix from object
    mat <- m$get()
    
    ## calculate the inverse
    inv_matrix <- solve(mat, ...)
    
    # set the inverse to the object
    m$setInverse(inv_matrix)
    
    inv_matrix
}
