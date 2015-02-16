## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
  {
  ## set mat_val null
  mat_val <- c()
  set <- function(y) 
    {
    ## set the values of x and mat_val in global enviroment
    x <<- y
    mat_val <<- c()
    }
  get <- function() x
  set_inv <- function(val2solve) mat_val <<- val2solve
  get_inv <- function() mat_val
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) 
  {
  ## Return a matrix that is the inverse of 'x'
  mat_val <- x$getinv()
  if(!is.null(mat_val)) 
    {
    message("getting cached data")
    return(mat_val)
    }
  my_data <- x$get()
  ## Computing the inverse of a square matrix can be done with the solve function in R.
  ## Example, if X is a square invertible matrix, then solve(X) returns its inverse.
  mat_val <- solve(my_data, ...)
  x$setinv(mat_val)
  mat_val
}
