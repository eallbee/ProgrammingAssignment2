## Put comments here that give an overall description of what your
## functions do



makeCacheMatrix <- function(x = matrix()) 
{
    ## This function creates a special matrix object that can cache its inverse
  m <- NULL
  set <- function(y) 
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get, 
        setmatrix = setmatrix, 
       getmatrix = getmatrix)
}


cacheSolve <- function(x, ...) 
{
    m <- x$getmatrix()
    if(!is.null(m))
    {
      message("getting cached data")
      return(m)
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m
    
    ## Return a matrix that is the inverse of 'x'
}

