## The following functions allows you to cache the inverse of matrix so that should you need
## the inverse going forward you can just call it

## The following function initializes a new matrix and returns a list
## of functions allowing you to get the matrix, set or change the matrix,
## set or change the inverse of the matrix and finally get the inverse of the matrix.
## When a new matrix is initialized, the inverse is automatically reset to NULL
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        
        get <- function() x
        
        set_inverse <- function(inverse) i <<- inverse
        
        get_inverse <- function() i
        
        list(set         = set,
             get         = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}

## This function first checks if the inverse already exists, and if it does
## it returns the cached inverse to save computing time, if the inverse has
## not been computed yet, the inverse is calculated and cached
cacheSolve <- function(x, ...) {
        i <- x$get_inverse()
        
        if(!is.null(i)){
                message("getting cached inverse")
                return(i)
        }
        
        data <- x$get()
        i    <- solve(data)
        x$set_inverse(i)
        i
}



