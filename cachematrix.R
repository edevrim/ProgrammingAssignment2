
##creates a matrix that can cache the inverse 
makecachematrix <- function(x = matrix()) {
        invs <- NULL 
        set <- function(y) {
                x <<- y
                invs <<- NULL 
                
        }
   get <- function() x 
   setinverse <- function(invss) invs <<- invss
   getinverse <- function() invs
   list(
           set = set, 
           get = get,
           setinverse = setinverse, 
           getinverse = getinverse
   )
        
        
}

##computes the inverse of the matrix created by above func 
##and if the inverse has already been calculated, 
##it changes back to the inverse from the cache

cacheSolve <- function(x, ...) {
        ## returns a matrix that is the inverse of 'x'
        invs <- x$getInverse()
        if (!is.null(invs)) {
                message("cached data")
                return(invs)
        }
        mat <- x$get()
        invs <- solve(mat, ...)
        x$setInverse(invs)
        invs
}
