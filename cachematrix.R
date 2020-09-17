## Put comments here that give an overall description of what your
## functions do

##function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  MC<- NULL
  set <- function(y){
    x <<- y
    MC<<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) MC<<- inverse
  getInverse <- function() MC
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)

}

 
##below function computes the inverse of the matrix returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Mc <- x$getInverse()
  if(!is.null(MC)){
    message("getting cached data")
    return(MC)
  }
  mat <- x$get()
  MC <- solve(mat,...)
  x$setInverse(MC)
  MC
}
