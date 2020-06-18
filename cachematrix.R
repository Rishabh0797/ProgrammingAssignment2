## Here function take input as a x=matrix

makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  set <- function(y) {
    x <<- y
    a <<- NULL
  }
        ## get is use to call matrix in next function
  get <- function() x
  setInverse <- function(inverse) a <<- inverse
  getInverse <- function() a
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  a <- x$getInverse()
        ## checking for value in matrix
  if(!is.null(a)) {
    message("getting cached data")
    return(a)
  }
  mat <- x$get()
  a <- solve(mat, ...)
        ## inversing the matrix
  x$setInverse(a)
  a
}
