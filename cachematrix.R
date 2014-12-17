## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# 1  set the value of the matrix
# 2  get the value of the matrix
# 3  set the value of the inverse
# 4  get the value of the inverse
## 

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
         x <<- y
         i <<- NULL
    }
    get <- function() x
    setinv <- function(solve) i <<- solve #compute the inverse matrix
    getinv <- function() i
    list( set = set, 
         get = get, 
         setinv = setinv, 
         getinv = getinv )

}


##  cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i<- x$getinv()
    
    if(!is.null(i)) { 
          # if inverse has been already calc-d then get data from cache 
          message("Getting Cache")
          return(i)
    }
    #if inverse is not computed then calculating inverse matrix and setting value in cache in x$setinv(i)
    data <- x$get()
    i<- solve(data, ...)
    x$setinv(i)
    i
}
