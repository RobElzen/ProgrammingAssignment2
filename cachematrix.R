makeCacheMatrix <- function(x = matrix()) {
        m <- NULL # sets the value of m to NULL (provides a default if cacheSolve has not yet been used)
        y <- NULL # sets the value of y to NULL (provides a default if cacheSolve has not yet been used)
        setmatrix <- function(y) { #set the value of the matrix
                x <<- y #caches the inputted matrix so that cacheSolve can check whether it has changed (note this is within the setmatrix function)
                m <<- NULL # # sets the value of m (the matrix inverse if used cacheSolve) to NULL
        }
        getmatrix <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(setmatrix = setmatrix, getmatrix = getmatrix, # creates a list to house the four functions
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function (x, ...) {
        # Need to compare matrix to what was there before!
        m <- x$getinverse() # if an inverse has already been calculated this gets it
        if(!is.null(m)){ # check to see if cacheSolve has been run before
                message("getting cached data")
                return(m)
        }
        # otherwise 
        data <- x$getmatrix() # run the getmatrix function to get the value of the input matrix
        m <- solve(data, ...) # compute the value of the inverse of the input matrix
        x$setinverse(m) # run the setinverse function on the inverse to cache the inverse
        m # return the inverse
}