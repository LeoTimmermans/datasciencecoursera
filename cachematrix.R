## makeChacheMatrix makes a list of functions to be uses by 
## cacheSolve to set the inverted matrix in cache and retrieve
## the inverted matrix from cache

## Create a matrix in cache

makeCacheMatrix <- function(x = matrix()) {
        ## initialize to NULL
        m <- NULL
        
        ## create matrix in working environment
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        
        ## retrieve matrix in working envorinment
        get <- function() x
        ## invert the matrix and store in cache
        setInverse <- function(solve) m <<- solve
        ## retrieve the inverted matrix from cache
        getInverse <- function() m
        
        ## return the created functions to the working environment
        list(set = set, get = get, setInverse = setInverse, 
             getInverse = getInverse)
        
}


## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Try to retrieve the inverted matrix from cache
        m <- x$getInverse()
        
        ## Test if the matrix is available in cache
        ## when available display message and display the inverted matrix
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## create inverted matrix when the inverted matrix does not exist
        data <- x$get()
        
        ## set and return the inverted matrix
        m <- solve(data, ...)
        x$setInverse(m)
        return(m)
}


# trial-run example:
# > c <- makeCacheMatrix()
# > c$set(matrix(1:4,2,2))
# > c$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheSolve(c)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(c)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5