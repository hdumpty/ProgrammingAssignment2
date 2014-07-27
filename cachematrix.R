## The below two functions allow to cache the matrix and its inverse and recalculate the inverse only when necessary


## makeCacheMatrix function creates a list of function to store and return matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverted <- NULL
        set <- function(y) {
                x <<- y
                inverted <<- NULL
        }
        get <- function() x
        setinverted <- function(i) inverted <<- i
        getinverted <- function() inverted
        list(set = set, get = get,
             setinverted = setinverted,
             getinverted = getinverted)
}



## cacheSolve function calculates the inverse matrix if it has not been done before, otherwise returns the cashed result

cacheSolve <- function(x, ...) {
        i <- x$getinverted()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverted(i)
        i
}
