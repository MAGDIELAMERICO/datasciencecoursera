
##This function Function makeCacheMatrix gets a matrix as an input

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  
  #set the value  
  setMatrix <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  
  getMatrix <- function() x                              
  setInverse <- function(inverse) invMatrix <<- inverse  
  getInverse <- function() invMatrix                     
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)
  
}


## The function cacheSolve takes the output of the previous matrix 

## Describing this function

cacheSolve <- function(x, ...) {
  
  #get the value of the invertible matrix from the makeCacheMatrix function
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)) {                       
    message("Getting Cached Invertible Matrix")   
    return(invMatrix)                             
  }
  
  #if value of the invertible matrix is NULL then  
  MatrixData <- x$getMatrix()                     
  invMatrix <- solve(MatrixData, ...)             
  x$setInverse(invMatrix)                         
  return(invMatrix)                               
  ## Return a matrix that is the inverse of 'x'
}

##Test 1 
TestMatrix <- matrix(1:4,2,2)
TestMatrix

CacheMatrix <- makeCacheMatrix(TestMatrix)
CacheMatrix$getMatrix()
CacheMatrix$getInverse()

cacheSolve(CacheMatrix)
cacheSolve(CacheMatrix)


##Test 2 
TestMatrix <- matrix(c(1,5,8,2),2,2)
TestMatrix

CacheMatrix <- makeCacheMatrix(TestMatrix)
CacheMatrix$getMatrix()
CacheMatrix$getInverse()

cacheSolve(CacheMatrix)
cacheSolve(CacheMatrix)


##Test 3
#matrix(1:9,3,3) is not possible (singule matrix) because it is giving det(A)  = 0
#matrix = 1/det(A)[3,-6,3,-6,12,-6,3,-6,3] |det(A) = 1/(1*3 +4* (-6) + 7 *3) = 1/0
TestMatrix <- matrix(1:9,3,3)
TestMatrix

CacheMatrix <- makeCacheMatrix(TestMatrix)
CacheMatrix$getMatrix()
CacheMatrix$getInverse()

cacheSolve(CacheMatrix)
cacheSolve(CacheMatrix)

##Test 4 
TestMatrix <- matrix(1:8,3,3)
TestMatrix

CacheMatrix <- makeCacheMatrix(TestMatrix)
CacheMatrix$getMatrix()
CacheMatrix$getInverse()

cacheSolve(CacheMatrix)
cacheSolve(CacheMatrix)


##Test 5 
TestMatrix <- matrix(c(2,3,5,1,3,7,4,5,6,8,0,0,4,5,6,0),4,4)
TestMatrix

CacheMatrix <- makeCacheMatrix(TestMatrix)
CacheMatrix$getMatrix()
CacheMatrix$getInverse()

cacheSolve(CacheMatrix)
cacheSolve(CacheMatrix)


##Test 6 
TestMatrix <- matrix(5:21,4,4)
TestMatrix

CacheMatrix <- makeCacheMatrix(TestMatrix)
CacheMatrix$getMatrix()
CacheMatrix$getInverse()

cacheSolve(CacheMatrix)
cacheSolve(CacheMatrix)