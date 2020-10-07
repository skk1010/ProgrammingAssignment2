## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        ## Initiating the inverse matrix
        
     inverse <- NULL
        
        ## setting up Inverse function
        
        set <- function (matrix) {
                matrix1 <<- matrix
                inverse <<- NULL
             }
         ## getting the Inverse function
        
        get <- function() {
                matrix1
             }
        
        ## setting up Inverse function
       setInverse <- function(inverse)  {
               inverse1 <- inverse
             }
      
        ## getting up Inverse function
        getInverse <- function() {
               inverse1
             }
}

    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    matrix <- x$getInverse()

    ## Just return the inverse if its already set
    if( !is.null(matrix) ) {
            message("getting cached data")
            return(matrix)
    }

    ## Get the matrix from our object
    data <- x$get()

    ## Calculate the inverse using matrix multiplication
    matrix1 <- solve(data) %*% data

    ## Set the inverse to the object
    x$setInverse(matrix1)

    ## Return the matrix
    matrix1
}
