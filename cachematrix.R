## A pair of functions that cache the inverse of a matrix.
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function (x=matrix())
{
  inv <- NULL
  #set function
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  #get function returns x
  get <- function()x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list (set=set,get=get,setInverse=setInverse, getInverse=getInverse)

}
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x,...)
{
  ##Returns the inverse of x
  inv <- x$getInverse()
  if (!is.null(inv))
  {
    message("getting cached data")
    return (inv)
  }
  data <- x$get()
  inv <- solve(data)
  inv
}
#checking
m <- matrix(rnorm(16),4,4)
m1 <- makeCacheMatrix(m)
k <- cacheSolve(m1)
k