## @x in makeCacheMatrix: a square invertible matrix
## return: a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##         this list is used as the input to cacheSolve()

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  ## stores the cached value, initialize to NULL 
  inv <- NULL
  set <- function(y) {
    ## use `<<-` to assign a value to an object in the working environment 
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse 
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve calculates the inverse of the matrix created in makeCacheMatrix 
## if the inverted matrix does not exist in makeCacheMatrix 
## it is created in the working environment and it's inverted value is stored in cache

cacheSolve <- function(x, ...) {
  ## @x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  
  inv <- x$getinv() #get the inversed matrix from object x 
  
  # if the inverse has already been calculated before 
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv) # return the calculated inversion 
  } 
  
  # otherwise, we do x$get to get the matrix object and calculate the inverse 
  mdata <- x$get()
  inv <- solve(mdata, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv) 
}


## testing my function 
test_matrix <- makeCacheMatrix(matrix(1:4, 2,2))
test_matrix$get()
test_matrix$getinv() 
cacheSolve(test_matrix)
cacheSolve(test_matrix)
test_matrix$getinv() 

test_matrix$set(matrix(c(2,9,3,8), 2,2))
test_matrix$get() 
test_matrix$getinv() 
cacheSolve(test_matrix) 
cacheSolve(test_matrix)
test_matrix$getinv()