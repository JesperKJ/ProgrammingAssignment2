## Following two functions used to calculate the inverse of a matrix
## In order to speed up computation time the inverse is saved in a global
## variable that can be accesed in environments other than it's own.
## Two functions are defined makeChacheMatrix and cacheSolve.  and the second function calls the cashed 
## value of the inverted matrix if it exists or calculates the inverse. 

## The first function is a list of functions setting and getting 
## a matrix and the inverse of a matrix (x)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

## The function cacheSolve checks if there is a saved value of the inverted matrix
## and uses the stored value if it is present. Otherwise it calculates the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if (!is.null(m)){
    message("getting stored inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}