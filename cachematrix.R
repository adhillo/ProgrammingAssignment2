## makeCacheMatrix is a function which returns a list of functions
## that allow an object to behave like a matrix

##cacheSolve checks for a cached solution, otherwise it calculates 
##the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setinv <- function(inv) m <- inv
    getinv <- function() m
    
    list(set = set, get = get, setinv = setinv,getinv = getinv)
    
}


cacheSolve <- function(x, ...) {
        
    m <- x$getinv()
    
    if (!is.null(m)) {
        
        message("getting cached value")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data)
    x$setinv(m)
    m 
}
