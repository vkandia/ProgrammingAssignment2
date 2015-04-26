## The following functions create a special object that stores a matrix and
## caches its inverse.
## We assume that the matrix supplied is always invertible.
## VK, April 2015

## The function "makeCacheMatrix" creates a list of functions which 
## elements are:
# 1. a function that sets the matrix (setmatrix)
# 2. a function that gets the matrix (getmatrix)
# 3. a function that sets the inverse of the matrix (setinverse)
# 4. a function that gets the inverse of the matrix (getinverse)

makeCacheMatrix <- function(x = matrix()) {
        inv<- NULL
        setmatrix <- function(y) {
                x <<- y
                inv <<- NULL
        }
        getmatrix<- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse, getinverse = getinverse)

}


## The following function ("cacheSolve") calculates the inverse matrix 
## of the one created with the above function. Firstly, it checks 
## if the inverse has already been calculated. If so, it gets it 
## from the cache and skips the calculation. Otherwise, it calculates 
## the inverse matrix and stores it in the cache with setinverse().

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv<- x$getinverse()
        #print(inv)
        
        if(!is.null(inv)){
                message("getting cached matrix")
                return(inv)
        }
        
        matrix<- x$getmatrix()
        inv<- solve(matrix, ...)
        x$setinverse(inv)
        inv
}
