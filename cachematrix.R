## Catching Inverse of Matrix

## Cache Matrix returns earlier computed Matrix if the same matrix is 
## passed to function again. 

## Input Parameter - Irreversible Matrix
## Output Parameter - List of functions containing Set, Get, SetInverse, 
## GetInverse functions
makeCacheMatrix <- function(x = matrix()) {

  
  InverseMatrix <- NULL
  
  ##Set the Matrix
  Set<- function(NewMatrix){
    
    x <<- NewMatrix
    InverseMatrix <<- NULL
  }
  
  ##Get the Matrix
  Get<- function() x  
  
  ##Set the Inverse Matrix
  SetInverse <- function(MatrixInverse){
    InverseMatrix <<- MatrixInverse
  }
  
  ##Get Inverse Matrix
  GetInverse <- function() InverseMatrix
  
  ##Return List of functions for Get Set of Matrix 
  ##and its Inverse Matrix
  list(set = Set, 
       get = Get, 
       setinverse = SetInverse, 
       getinverse = GetInverse)
}
  




## CacheSolve

## For new Matrix computes the Inverse Matrix and reurns earlier computated Inverse MAtrix
## if calculated earlier maintained in cache.

## Input Parameter - List of functions getting used for computation of Inverse Matrix 
## Output Parameter - Matrix inverse of passed Matrix 

cacheSolve <- function(MatrixList, ...){
  
  ##Get the Inverse Matrix
  InverseMatrix <- MatrixList$getinverse()
  
  ## Get the Cached Matrix if the Inverse Matrix is not NULL
  if(!is.null(InverseMatrix)){
 
    message("getting cached data")
    return(InverseMatrix)
    
  }else
  {
  ## Compute the Inverse Matrix and set it.
    Matrix <- MatrixList$get()
    InverseMatrix <- solve(Matrix)
    MatrixList$setinverse(InverseMatrix)
    InverseMatrix
  }
}

