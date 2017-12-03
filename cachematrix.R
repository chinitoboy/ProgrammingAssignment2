
## Similar with previous example...

# The first function, `makeCacheMatrix()` creates a special "matrix" 
# object that can cache its inverse

#input: square matrix
#1.  set the matrix
#2.  get the matrix
#3.  set the inverse
#4.  get the inverse
#output: list of item 1-4

makeCacheMatrix <- function(x = matrix()) {
  # initialize inverse 
  inv <- NULL
  # set matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # get matrix
  get <- function() x
  # set inverse
  setinverse <- function(inverse) inv <<- inverse
  # get inverse
  getinverse <- function() inv
  # outputs to cacheSolve
  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}

# The second function, `cacheSolve()` computes the inverse of the 
# special "matrix" returned by makeCacheMatrix above. If the inverse 
# has already been calculated (and the matrix has not changed), then 
# cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  # look cache getinverse
  inv <- x$getinverse()
  # message if already answered inverse
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv) # skips computation
  }
  # Computing inverse of  matrix
  data <- x$get()
  inv <- solve(data, ...)
  # Set value of inverse in cache
  x$setinverse(inv)
  inv
}

## Sample run:
# x <- matrix(c(1:4), nrow = 2)
# y <- makeCacheMatrix(x)
# cacheSolve(y) # trial 1
# cacheSolve(y) # trial 2
 


