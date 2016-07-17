## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix sets the value of the matrix, gets the value of the matrix, sets the value of the inverse of the matrix and gets the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}




## Write a short comment describing this function
##The following function returns the inverse of the matrix. It first checks if the inverse has already been computed. If so, it gets the result and skips the computation. If not, it computes the inverse, sets the value in the cache via setinverse function.


cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

        ## Return a matrix that is the inverse of 'x'
        ## x=rbind(c(1,2),c(2,1))
        ## m=makeCacheMatrix(x)
        ## m$get()
        ##[,1] [,2]
##[1,]    1    2
##[2,]    2    1
        ## No cache in the first run
        ## cacheSolve(m)
        ##[,1]       [,2]
##[1,] -0.3333333  0.6666667
##[2,]  0.6666667 -0.3333333
        ## Retrieve from the cache in the second run
        ## cacheSolve(m)
        ##getting cached data
        ##[,1]       [,2]
##[1,] -0.3333333  0.6666667
##[2,]  0.6666667 -0.3333333
##> 

