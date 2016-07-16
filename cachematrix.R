## Matrix inversion is usually a costly computation
## and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of inverse of the matrix
## get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) cache <<- inverse
  getInverse <- function() cache
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}

## cacheSolve function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        dataMatrix <- x$get()
        inverse <- solve(dataMatrix, ...)
        x$setInverse(inverse)
        inverse
}


## Example :

## > data_matrix <- makeCacheMatrix(matrix(c(2,7,9,11), 2, 2))

## > data_matrix$get()
##      [,1] [,2]
## [1,]    2    9
## [2,]    7   11

## > data_matrix$getInverse()
## NULL

## > cacheSolve(data_matrix)
##            [,1]        [,2]
## [1,] -0.2682927  0.21951220
## [2,]  0.1707317 -0.04878049

## > cacheSolve(data_matrix)
## getting cached data
##            [,1]        [,2]
## [1,] -0.2682927  0.21951220
## [2,]  0.1707317 -0.04878049

## > data_matrix$getInverse()
##            [,1]        [,2]
## [1,] -0.2682927  0.21951220
## [2,]  0.1707317 -0.04878049
