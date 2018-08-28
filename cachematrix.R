## Code for Programming Assignment 2 in 
## Coursera: R Programming (Part 2 of Data Science) (CSRDATA331)
## Overall assignment is to create two functions
## First is makeCacheMatrix which creates a special
## matrix object that can cache its own inverse
## Second is cacheSolve which computes the inverse of the
## special matrix, returning the cached value of the inverse
## matrix if it has been computed and the matrix has not changed.

## makeCacheMatrix
## is really a list containing a function to
## setmat - sets the value of the matrix and sets element[1,1] of its inverse to NA
## getmat - gets the value of the matrix
## setinv - caches the inverse matrix in minv
## getinv - retrieves the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        ## create the inverse matrix
        ## values will be NA
        minv <- matrix(nrow = dim(x)[1], ncol = dim(x)[1])
        
        ## set the matrix to be inverted
        ## flag that the matrix has not been inverted yet
        ## will be that element [1,1] is NA
        setmat <- function(y) {
                x <<- y
                minv[1,1] <<- NA
        }
        
        ## return the matrix
        getmat <- function() x
        
        ## cache the inverse matrix
        setinv <- function(soln) minv <<- soln
        
        ## retrieve the cached inverse matrix
        getinv <- function() minv
        
        ## return the list
        list(setmat = setmat, getmat = getmat,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve calculates the mean of the special matrix created
## with makeCacheMatrix.
## It first tests if the matrix has been inverted already and returns the
## cached inverse if it has.
## Otherwise, it inverts the matrix and caches the result before returning it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## if there a cached solution and if it is the inverse
        ## of x. Solve otherwise and 
        ## cache the result
        
        ## get the cached result
        minv <- x$getinv()
        
        ## if there is a cached inverse, element [1,1] will NOT be NA
        if(!is.na(minv[1,1])) {
                ## element[1,1] was not NA
                ##there is a cached matrix
                message("getting cached inverse")
                ## return the cached matrix
                return(minv)
        }
        
        ## there was no cached matrix
        ## get the matrix to be inverted
        ## no error checking - assume the matrix is square and invertable
        mdata <- x$getmat()
        
        ## invert the matrix with the solve function
        ## diag(dim(mdata)[1]) returns the identity matrix of the
        ## same dimension as the matrix to be inverted
        minv <- solve( mdata, diag( dim(mdata)[1] ) )
        
        ## cache the result
        x$setinv(minv)
        
        ## return the inverted matrix
        minv
}
