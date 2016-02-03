# ===================
# This set of functions are helpful in caching the inverse of a matrix 
# instead of computing it repeatedly to reduce the computation costs 
# ===================

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  mInv <- NULL
  
  setMatrix <- function(y) {
    x <<- y
    mInv <<- NULL
  }
  getMatrix <- function() x
  
  setMatrixInverse <- function(matrixInverse) mInv <<- matrixInverse
  getMatrixInverse <- function() mInv
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix function. 
## If the inverse has already been calculated (and the matrix has not changed), then this function should retrieve the inverse from the cache.
cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of x
  mInv <- x$getMatrixInverse()
  if(!is.null(mInv)) {
    message("getting cached data")
    return(mInv)
  }
  matData <- x$getMatrix()
  mInv <- solve(matData)
  x$setMatrixInverse(mInv)
  mInv
}

## Experiment to test cache process by print the computing time
nRow <- 1000
nCol <- 1000
set.seed(1020323)
randomArray = rnorm(nRow*nCol)
x = matrix(randomArray, nRow, nCol)

# first time of computing matrix inverse
z = makeCacheMatrix(x)
start.time = Sys.time() 
cacheSolve(z)
runTime = Sys.time() - start.time
print(runTime)

# second time of compute matrix inverse (directly use the matrix inverse in the cache/environment)
start.time = Sys.time()
cacheSolve(z)
runTime = Sys.time() - start.time
print(runTime)

# learning Notes: 
# for the above test case, the first time takes 2.5sec, and second time takes 0.0007 sec. 
# The cache idea could save a lot of computing time for large dataset!