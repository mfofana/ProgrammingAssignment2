## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
