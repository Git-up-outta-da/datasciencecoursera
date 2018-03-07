## These two functions will, firstly, allow for a matrix to be stored in the cache
## and secondly, draw this matrix from the cache and compute its inverse using the solve function

## Based off the sample function for caching a vector provided on coursera the necessary adjustments
## have been made to apply the same idea to a matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL

}

    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## using the sample function provided on coursera to calculate the mean from a cached vector
## this function will compute the inverse of a cached matrix

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
            message("getting cached data.")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}


        ## Return a matrix that is the inverse of 'x'

# Make a matrix called X

x <-  cbind(c(50, 145,74), c(59,401, 10), c(500, 61, 37))
# > x
# [,1] [,2] [,3]
# [1,]   50   59  500
# [2,]  145  401   61
# [3,]   74   10   37

## Store matrix in cache

m <-  makeCacheMatrix(x)

I <- cacheSolve(m)

# > I
# [,1]          [,2]          [,3]
# [1,] -1.057702e-03 -0.0002094290  0.0146385446
# [2,]  6.326733e-05  0.0026132160 -0.0051632390
# [3,]  2.098305e-03 -0.0002874166 -0.0008545923

