#The following functions are used to create a special object that stores a matrix and caches its inverse.
#The first function, makeCacheMatrix creates a special “matrix”, which is really a list containing a function to:

#1.set the value of the matrix

#2.get the value of the matrix

#3.set the value of the inverse

#4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
  set <- function(y) {
          x <<- y
          i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
#This function computes the inverse of the special “matrix” returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

#Here we are going to take an example to see how it looks when it is computed.
#Below we call the function with a matrix, compute the inverse, retrieve the inverse from the cache list, change the call matrix to the inverse, compute the inverse on that and return the original function.

#B <- matrix(c(1,2,3,4),2,2)
#solve(B) #We pretend that this cant't happen xD
#B1 <- makeCacheMatrix(B)
#cacheSolve(B1) #inverse returned after computation
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
#cacheSolve(B1) #inverse returned from cache
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
