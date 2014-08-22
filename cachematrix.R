## Functions calculates the inverse matric of the input matrix. It first checks if the 
## inverse matrix has already been calculated. If so, it gets the inverse from the cache and 
## skips the computation. Otherwise, it calculates the inverse matrix of the data and sets
## the value  in the cache via the setinverse function.


## This finction create a special "vector" , which is really a list containing 
## a function to : set the value of the matrix,get the value of the matrix,
## set the value of the inverse matrix, get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function first checks if the inverse matrix has already been 
## calculated in function above. If so, it gets the inverse from the cache and 
## skips the computation. Otherwise, it calculates the inverse matrix and sets
## the value  in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
       
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


