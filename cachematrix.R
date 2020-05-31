## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix(x) creates a matrix with its inverse embedded/cached in itself
## cacheSolve(x, ...) returns inverse of the matrix 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  ## a null matrix assigned to inverse matrix variable  
  inv_matrix <- matrix()
  
  ## set the matrix with setMatrix function
  setMatrix <- function(y) {
    x <<- y
    inv_matrix <<- matrix()
  }
  
  ## get Matrix with getMatrix method
  getMatrix <- function() x
  
  ## set the Inverse of the matrix
  setInverseMatrix <- function() {
    
    inv_matrix <<- solve(x)
    
  }
  
  getInverseMatrix <- function() inv_matrix
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverseMatrix = setInverseMatrix
       getInverseMatrix = getInverseMatrix)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix <- x$getInverseMatrix()
        
        if( !(is.null.DN(dimnames(inverseMatrix))) ) {
           message("getting cached inverse matrix")
           return(inverseMatrix)
        }
        
        matrix1 <- x$getMatrix()
        inverseMatrix <- solve(matrix1, ...)
        x$setInversetMatrix(inverseMatrix)
        inverseMatrix
}
