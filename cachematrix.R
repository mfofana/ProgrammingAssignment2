##  Pair of functions that cache the inverse of a matrix rather than compute it repeatedly 

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

        inverse_matrix <- NULL
    
        mat <<- x
  
        set <- function(y) {
    
                stopifnot(is.matrix(y))
                mat <<- y
                inverse_matrix <<- NULL
        }
        
        get <- function() mat
  
        setinverse_matrix <- function(imat) {
                inverse_matrix <<- imat
        }
  
        getinverse_matrix <- function() inverse_matrix
  
        list(set = set, get = get,
                setinverse_matrix = setinverse_matrix,
                getinverse_matrix = getinverse_matrix)
  

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        inverse_x <- x$getinverse_matrix()

        if(!is.null(inverse_x)) {
                message("getting cached x")
                return(inverse_x)
        }
        #retrieve   
        this_x <- x$get()
        
        inverse_x <- solve(this_x)
        
        x$setinverse_matrix(inverse_x)
        
        inverse_x
}
