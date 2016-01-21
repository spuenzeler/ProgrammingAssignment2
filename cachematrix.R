## makeCacheMatrix caches matrices that have been inverted by cacheSolve. It passes four functions to cacheSolve.
## cacheSolve takes matrices from makeCacheMatrix and inverts them (solve()) if they had not been inverted
## before; in this case cacheSolve just returns the cached inverted matrix stored in makeCacheMatrix.



## makeCacheMatrix provides 4 functions. (get) returns a passed matrix. (getinvere) returns an inverted matrix.
## (set) changes the stored input (x) matrix and resets the variable that stores the inverted (i) matrix within 
## the makeCacheMatrix environment. (setinverse) stores the inverted matrix.      

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) i <<- inverse
        
        getinverse <- function() i
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
        
}


##cacheSolve calls makeCacheMatrix to receive a matrix (or a stored inverted matrix). If the matrix was stored before,
##a message is dropped (see below). Otherwise the matrix is inverted with the solve function and printed to the console.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}