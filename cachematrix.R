# This set of functions, when provided an invertable matrix, will 
# return the inverse of that matrix, and cache its results for future
# reference.  If the inverse of a previously-supplied matrix is supplied, 
# the function will return its cached inverse result rather than consuming
# computing power to perform a recalculation.
#
# Full 2x2 Example:
# > source("cacheMatrix.R")
# > a <-makeCacheMatrix(matrix(c(4,3,3,2),ncol=2,byrow=T)
# > cacheSolve(a)
# [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4
# > cacheSolve(a)
# getting cached data
# [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4


#   Purpose: Create a special matrix that can cache its inverse
#     Usage: makeCacheMatrix(x)
# Arguments: x         a matrix object
#   Outputs: none

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinversematrix <- function(solve) m <<- solve
      getinversematrix <- function() m
      list(set = set, get = get,
           setinversematrix = setinversematrix,
           getinversematrix = getinversematrix)
}


#   Purpose: computes the inverse of the special "matrix" returned 
#            by `makeCacheMatrix` above. If the inverse has
#            already been calculated (and the matrix has not changed), then
#            `cacheSolve` should retrieve the inverse from the cache
#     Usage: cacheSolve(x, ...)
# Arguments: x         a makeCacheMatrix object
#   Outputs: a inverse matrix object

cacheSolve <- function(x, ...) {
      m <- x$getinversematrix()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinversematrix(m)
      m
}
