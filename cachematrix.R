## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Function to set and get the value of a matrix and to set and get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        
        setinv <- function(inversematrix){
                inv <<- inversematrix
        }
        getinv <- function(){inv}
        
        list(set = set, get= get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## Funtion to calculate the inverse of a matrix using the above function
## It first checks if the inverse exists in cache and the matrix is unchanged. It gets the inverse from cache and skips the calculation 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
                message("Getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat,...)
        x$setinv(inv)
        inv
}
