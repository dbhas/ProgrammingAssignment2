## Below are two functions that are used to create 
## a special function that stores a matrix object
## and cache its inverse.  

## The first function creates a list containing a
## function to get/set the matrix and
## to get/set the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The following function calculates the inverse of 
## a matrix with the above function. It first checks
## to see if the inverse has already been calculated.
## If so, it  gets the inverse from the cache and
## skips computation. Otherwise, it calculates the
## inverse of the matrix and stores the matrix in the
## cache via the setInverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}

## Sample output:
# > b<-matrix(c(1,-1,1,-1,2,1,-1,3,4),3,3)
# > mat.b<-makeCacheMatrix(b)
# > mat.b$get()
# [,1] [,2] [,3]
# [1,]    1   -1   -1
# [2,]   -1    2    3
# [3,]    1    1    4
# > cacheSolve(mat.b)
# [,1] [,2] [,3]
# [1,]    5    3   -1
# [2,]    7    5   -2
# [3,]   -3   -2    1
# > cacheSolve(mat.b)
# getting cached data
# [,1] [,2] [,3]
# [1,]    5    3   -1
# [2,]    7    5   -2
# [3,]   -3   -2    1