## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##It has four functions: set method= update the new value of matrix
## get method= obtain the current value of the matrix
## setinverse and getinverse= does the same but with the inverse matrix

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


## Write a short comment describing this function

## it takes the list returned from makeCacheMatrix which is four function and 
## the value of matrix .
## it checks if the inverse is already present if yes then return the inverse 
##of the matrix otherwise calulate it and update the values accordingly and also
## return the value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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


## Checking Example
cacheSolve(makeCacheMatrix(x=as.matrix(cbind(c(3,0),c(1,2)))))
