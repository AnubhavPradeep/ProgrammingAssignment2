## Assignment is to write a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
#Comm1: makeCacheMatrix is a function that stores a list of functions

#Comm2:  "x <<- y" substitutes the vector x with y (the input) in the main function (makeCacheMatrix).
#Comm2:  If it was "x <- y" it would substitute the matrix x with y only in the set function.

#Comm3: "m <<- NULL" restores to null the value of the inverse m, because the old inverse of the old matrix is not needed anymore.

#Comm4: 'setinverse' and 'getinverse' simply store the value of the input in a variable m into the main function makeCacheMatrix
#Comm4: (setinverse) and return it (getinverse).

makeCacheMatrix <- function(x = matrix()) {
              m <- NULL
              set <- function(y){
                      x <<- y
                      m <<- NULL
              }
              get <- function() x
              setinverse <- function (solve) m <<- solve
              getinverse <- function() m
              list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

#Comm1: The first thing cacheSolve does is to verify the value m, stored previously with getinverse, 
#Comm1: exists and is not NULL. If it exists in memory, it simply returns a message and the value m, 
#Comm1: that is supposed to be the inverse, otherwise, data gets the vector stored with makeCacheMatrix, 
#Comm1: m calculates the inverse of the vector and x$setinverse(m) stores it in the object generated 
#Comm1: assigned with makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
              m <- x$getinverse()
              if(!is.null(m)){
                        message("getting cached data")
                        return(m)
              }
              data <- x$get()
              m <- solve(data,...)
              x$setinverse(m)
              m
}
