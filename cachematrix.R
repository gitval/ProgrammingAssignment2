## creates a matrix whose inverse can be cached and later retrieved, 
## and creates the function to act on that matrix to calculate its inverse

## takes as input any matrix 'm' and creates a cacheable matrix
## 

makeCacheMatrix <- function(m = matrix()) {
        i <- NULL
        set <- function(y){
            m <<- y
            i <<- NULL
        }
        get <- function() m
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list (set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)

}


## returns the inverse of a matrix 'x', using cached inverse calculation if available

cacheSolve <- function(m, ...) {
        i <-m$getinverse()
        if(!is.null(i)) {
              message("getting cached data")
              return(i)
        }
        data <-m$get()
        i <- solve(data, ...)
        m$setinverse(i)
        return(i)
}
