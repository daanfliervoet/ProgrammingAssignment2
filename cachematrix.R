## This function creates a matrix. Furthermore, it can cache its inverse. 
## Used de solve() function to to compute the inverse of hte matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}

## This function will calculate the inverse of a mtrix created by the function makeCacheMatrix. If the inverse is
## calcucalted all ready by the fuction above than the fucntion will returm the inverse from the cache. In doing so
## it will show the message "getting cahced data"

cacheSolve <- function(x=matrix(), ...) {
        m <- x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setmatrix(m)
        m
}
