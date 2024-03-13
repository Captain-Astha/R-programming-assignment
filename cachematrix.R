## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse  <- NULL
  set <- function(y) 
  {
    x <<- y
    inverse <<- NULL
  }
  get <- function() 
  {
    x
  }
  
  setinverse <- function(inv)
  { 
    inverse <<- inv
  }
  getinverse <- function()
  {
    inverse
  } 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)



}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  
  if(!is.null(inverse)) 
  {
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

test <- function() 
{
  my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
  my_matrix$get()
  my_matrix$getinverse()
  cacheSolve(my_matrix)
  my_matrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
  cacheSolve(my_matrix)   # Computes, caches, and returns new matrix inverse
  my_matrix$get()         # Returns matrix
  my_matrix$getinverse()  # Returns matrix inverse    
  my_matrix$get() %*% my_matrix$getinverse() # returns the identity matrix
        ## Return a matrix that is the inverse of 'x'
}

