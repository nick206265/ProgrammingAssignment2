##Here are two functions
#that are used to create a special object that stores
#a matrix and caches its inverse.

#The first function, makeVector creates a special "matrix",
#which is really a list(!) containing a functions to

#set the value of the matrix
#get the value of the matrix
#set the value of the inverse matrix
#get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


#The following function calculates the inverse matrix of the special "matrix"
#created with the above function. However, it first checks
#to see if this matrix has already been calculated.
#If so, it gets the matrix from the cache and skips the computation.
#Otherwise, it calculates the inverse matrix and sets
#the value of the inverse matrix in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}