## These functions enable caching the inverse of a matrix 

## function returns a vector of four functions - set,get matrix and set,get matrix Inverse representing a matrix object
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrixinv <- function(matrixinv) m <<- matrixinv
    getmatrixinv <- function() m
    list(set = set, get = get, setmatrixinv = setmatrixinv,getmatrixinv = getmatrixinv)
}

##function returns a  Matrix inverse .if already computed, it returns the cached value

cacheSolve <- function(x, ...) {
    m <- x$getmatrixinv()
    if(!is.null(m)) {
        message("cached")
        return(m)
    }
    matrix <- x$get()
    m <- solve(matrix)
    x$setmatrixinv(m)
    m
}

