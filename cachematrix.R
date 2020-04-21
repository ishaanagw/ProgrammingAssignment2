##This is a function to calculate the inverse of a matrix.
##This function also stores the inverse of the matrix in the cache to avoid the complex programming 



## this function returns a special vector which can be given the value of the inverse to be 
##stored in the cache and also set the input of the matrix for which the inverse is to be calculated
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setmat <- function(y){
      x <<- y
      inv <<- NULL
      
    }
    getmat <- function()x
    setinv <- function(inv_mat)inv <<- inv_mat
    getinv <- function()inv
    list(set = setmat,get = getmat,
         setinverse = setinv, getineverse = getinv)
}


##This function is used to calculate the inverse and it also checks the if the
## inverse is already present calculated, if yes it returns that value. if the 
##inverse is not calculated the function calculates the inverse and stores it in the cache 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getineverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat)
  x$setinverse(inv)
  inv
}
