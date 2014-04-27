## The following functions(makeCacheMatrix and cacheSolve) aide 
## in making potentially time-consuming Matrix Inversion
## computations more efficient through applying a caching technique 
  

## The first funtion makeCacheMatrix takes an argument x of 
## type matrix and returns a list of 4 R functions. The list 
## can be accessed through the list access symbol '$ ' 
## for example: $get(),$set(),$getInverse(),$setInverse()
## which will be used by the CasheSolve function

makeCacheMatrix <- function(x = matrix()) {
        
        mx <- NULL
        
        set <- function(y) {
              x <<- y
              mx <<- NULL
          }
    
        get        <- function() x
        setInverse <- function(Inverse) mx <<- Inverse
        getInverse <- function() mx
        
          
        list(set = set, get = get,
        setInverse  = setInverse,
        getInverse  = getInverse )
}


## The second function cacheSolve takes as an argument 
## the output of the makeCacheMatrix function and outputs 
## the Matrix Inverse values.The outputs from the cacheSolve 
## function can result from calculating the inverse matrix or
## securing the cached data. This depends on if mx was previously 
## calculated or not

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      mx <- x$getInverse()
      
      if(!is.null(mx)) {
        message("getting cached data")
        return(mx)
      }
      
        data <- x$get()     # mx is null or not caluculted
                            # must therefore calulate the inverse
      mx <- solve(data)     # matix here and capture the results in x's
                            # cache then return results
      x$setInverse(mx)
      
      mx
} 
        
#credit to Fu Sheng Wang for great assignment explanations!

