## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
         m <- NULL                              ## sets the value of m to NULL (provides a default if cacheSolve has not yet been used)
        set <- function(y) {                    ##set the value of the matrix
                x <<- y                          ## caches the inputted matrix so that cacheSolve can check whether it has changed 
                m <<- NULL                      ## sets the value of m (the matrix inverse if used cacheSolve) to NULL
        }
        get <- function() x                             #getting the value of the matrix
        setmatrix <- function(solve) m <<- solve         #setting the value of the inverse of the matirx
        getmatrix <- function() m                       #getting the value of the inverse of the matrix
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
      m <- x$getmatrix()                                 ## if an inverse has already been calculated this gets it
        if(!is.null(m)) {                               ##check to see if cacheSolve has been run before
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()                                ## run the getmatrix function to get the value of the input matrix
        m <- solve(matrix, ...)                           ## compute the value of the inverse of the input matrix
        x$setmatrix(m)                                  ## run the setmatrix function on the inverse to cache the inverse
        m                                               ## return the inverse
}
