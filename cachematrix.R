

## The first function, makeCacheMatrix creates  list contains a function to:-

## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
## instead of 'solve' function 'ginv' can be used to find pseudo invers
## if matrix is non-square.
## Also 'solve' will not work if matrix is singular.

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




## The following function calculates the inverse of special "matrix" created above.But if
## inverse has allready been calculated then it gets the inverse from the cache and skips
## the computation.
## Otherwise, it calculates the inverse of the matrix and sets the matrix inverse in the 
## cache via the 'setinverse' function.

cacheSolve <- function(x, ...) {
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


# Example:-

## A= matrix(c(3,2,1, 1,2,3, 1,2,1), nrow=3)
## s=makeCacheMatrix(A)
## cacheSolve(s)
[,1]  [,2]          [,3]
[1,]  5.000000e-01 -0.25 -3.700743e-17
[2,]  2.081668e-17 -0.25  5.000000e-01
[3,] -5.000000e-01  1.00 -5.000000e-01
## > k=cacheSolve(s)
getting cached data
> k
[,1]  [,2]          [,3]
[1,]  5.000000e-01 -0.25 -3.700743e-17
[2,]  2.081668e-17 -0.25  5.000000e-01
[3,] -5.000000e-01  1.00 -5.000000e-01
## > k %*% A
[,1]          [,2]          [,3]
[1,] 1.000000e+00 -1.110223e-16 -3.700743e-17
[2,] 1.110223e-16  1.000000e+00  0.000000e+00
[3,] 0.000000e+00  0.000000e+00  1.000000e+00
