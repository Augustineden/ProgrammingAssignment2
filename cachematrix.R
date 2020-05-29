## The pair of functions below allow the value of the  inverse of a matrix to be 
## cached and then retrieved, which can save computation time in instances where 
## the inverse of the same matrix may need to be calculated more than once.

## This function creates a matrix which is a list containing 4 functions to

# 1: set the value of the matrix
# 2: get the value of the matrix
# 3: set the value of the inverse of the matrix
# 4: get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y = matrix()) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(solve) i <<- solve(x)
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

#===

## This function first of all checks to see if the inverse has already been calculated. 
## If so, it retrieves the inverse from the cache.
## If not, it calculates the inverse of the matrix created with the makeCacheMatrix function, 
## and then stores this value in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        print(data)
        i <- solve(data, ...)
        x$setinv(i)
        i
}