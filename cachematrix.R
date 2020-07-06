## These functions create a special matrix object that can be accessed from the
## global environment (?) using the superassignment <<- operator. We can then
## cache the inverse of this special matrix to reduce expensive computation time


## Create a special matrix and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    invMat <- NULL # set inverse matrix to NULL
    setMat <- function(y) { # use setMat function to 'reSET' the matrix
        x <<- y # overwrite the old matrix with the new matrix. We need to use the <<- operator in order to access the new matrix OUTSIDE the scope of the setMat function
        invMat <<- NULL # when we reset the matrix, delete the old inverse matrix by setting it to NULL
    }
    getMat <- function() x # this is just to return the current matrix
    setInvMat <- function(inverse) invMat <<- inverse # once the inverse gets calculated from the cacheSolve function, set the inverse matrix to that solution
    getInvMat <- function() invMat # gets the inverse matrix
    list(setMat = setMat,
         getMat = getMat,
         setInvMat = setInvMat,
         getInvMat = getInvMat)
    # this function returns a list
}

## cacheSolve checks if the special matrix has already been created. If not,
## we create the special matrix. If it has already been created, we return the
## cached inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invMat <- x$getInvMat()
    if(!is.null(invMat)) {
        message("Retreiving cached inverse matrix")
        return(invMat)
    }
    else {
    # should this be part of an 'else' statement? That would make sense given that the entire point of this is to avoid solving the matrix over an over again...
    data <- x$getMat()
    invMat <- solve(data, ...)
    x$setInvMat(invMat)
    invMat
    }
}
