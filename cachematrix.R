
## makeCacheMatrix creates a special "matrix" that might be cached with function cacheSolve

makeCacheMatrix <- function(x = matrix()) 
{
  inverse <- NULL
  set <- function(y) 
  {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse =  setinverse,
       getinverse = getinverse)

}

## cacheSolve calculates the inverse of the special "matrix" created with the function above

cacheSolve <- function(x, ...) 
{
  inv <- x$getinverse()
  if(!is.null(inv)) 
  {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse    
}
