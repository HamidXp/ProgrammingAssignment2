## Caching the Inverse of a Matrix
## Below are two functions that are used to create a special object that 
## stores a matrix and cache's its inverse.


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
		
    set <- function(y) {
        x     <<- y
        inv_x <<- NULL
    }
        
    get     <- function()  x
    set_inv <- function(X) inv_x <<- solve(x)
    get_inv <- function()  inv_x
		
    list(set     = set    ,
	 get     = get    ,
	 set_inv = set_inv, 
         get_inv = get_inv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    mat <- x$get_inv()
	
    if(!is.null(mat)) {
        message("getting cached data")
        return(mat)
    }
	
    data <- x$get()
    mat  <- solve(data)
    x$set_inv(mat)
    mat
}
