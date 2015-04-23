##Here is a pair of functions that cache the inverse of a matrix.

##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = Matrix()) {   #to make a function giev a result - list of object
  inv <- NULL                         #set m as null when no result come out 
  setMatrix <- function(y) {        #a function which set up for get a matrix
    x <<- y                       
    inv <<- NULL
  }
  getMatrix <- function() x      #get a matrix result by function about
  
  setinvMatrix <- function(z) inv <<- z #a function which set up for get a matrix for inverse
  getinvMatrix <- function() inv #get matrix for inverse result by function about
  list(setMatrix = setMatrix, getMatrix = getMatrix, # Return a list of function by makeCacheMatrix 
       setinvMatrix  = setinvMatrix ,
       getinvMatrix = getinvMatrix)
}

##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {     #use the makecachematrix function to get inverse matrix
   m<-x$getinvMatrix()               #get the inverse matrix form makeCacheMatrix
   if(!is.null(m)){                  #if m is not null, do following
     message("getting cached data")  # message"getting cached data" will showed when this is not the first run
     return(m)                      #return inverse matrix
  }
  
   matrix <- x$getMatrix()          # use when m is null 
   m<-solve(matrix, ...)           #inverse matrix
   x$setinvMatrix(m)              # get the result
   m
}
  

##   Example For Test and understanding:                 
##    x <- rbind(c(2,3), c(4,5))        ##set up a Matrix 
##    result <-makeCacheMatrix(x)         ##put the result of makeCacheMatrix into result   
##    result$getMatrix()                  ##show the orginal Matrix
##    result$getinvMatrix()               ##show the inverse Matrix --is null at this point, because not calculate yet
##    cacheSolve(result)                  ## show the inverse Matrix by CacheSolve 
##  result$getinvMatrix() %*% result$getMatrix()     ##check the result
