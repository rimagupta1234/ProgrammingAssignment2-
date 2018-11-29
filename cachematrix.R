## Matrix inversion is usually a costly computation and there may be some benefit to caching

## the inverse of a matrix rather than computing it repeatedly (there are also alternatives

## to matrix inversion that we will not discuss here).

## Your assignment is to write a pair of functions that cache the inverse of a matrix.



## 1) makeCacheMatrix: This function creates a special "matrix" object that can cache its 

#  inverse.

## 2)cacheSolve: This function computes the inverse of the special "matrix" returned by

## makeCacheMatrix above. 

## If the inverse has already been calculated (and the matrix has not changed), then

## cacheSolve should retrieve the inverse from the cache.


makeCacheMatrix <- function(x = matrix()) { ## define the argument w default mode = "matrix"
  
  inv <- NULL                             ## initialize inverse 
  
  set <- function(y) {                    ## define the set function
    
    x <<- y                             ## use '<<-' to assign a value of obj in env dif from current
    
    inv <<- NULL                        ## reset inv to NULL if there is a new matrix
    
  }
  
  get <- function() x                     ## return the value of the matrix
  
  
  
  setinverse <- function(inverse) inv <<- inv  <<- solve(x) ## calculate the inverse
  
  getinverse <- function() inv                     		  ## gets the value of inverse
  
  list(set = set, 			        ## you need this so functions can refer to $ operator
       
       get = get, 			
       
       setinverse = setinverse, 
       
       getinverse = getinverse)                                                                              
  
}







cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
}





## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

## If the inverse has already been calculated (and the matrix has not changed), then the 

## cachesolve should retrieve the inverse from the cache.



cacheSolve <- function(x, ...) {            ## return the inverse of the original matrix input to above
  
  inv <- x$getinverse()
  
  if(!is.null(inv)) {                     ## if the inverse has already been calculated
    
    message("getting cached data.")     ## get it from the cache and skips the computation.
    
    return(inv)
    
  }
  
  data <- x$get()			        ## otherwise, calculates the inverse
  
  inv <- solve(data,...)
  
  x$setinverse(inv)                       ## sets the value of the inverse in the cache
  
  inv
  
}



##testing##

##> source("cachematrix.R")

##> my_matrix<-makeCacheMatrix(matrix(1:4, 2, 2))

##> my_matrix$get()

##     [,1] [,2]

##[1,]    1    3

##[2,]    2    4

##> my_matrix$getinverse()

##NULL

##> cacheSolve(my_matrix)

##     [,1] [,2]

##[1,]   -2  1.5

##[2,]    1 -0.5

##> my_matrix$getinverse()

##     [,1] [,2]

##[1,]   -2  1.5

##[2,]    1 -0.5

##> my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))

##> my_matrix$get()

##     [,1] [,2]

##[1,]    2    1

##[2,]    2    4

##> my_matrix$getinverse()

##NULL

##> cacheSolve(my_matrix)

##           [,1]       [,2]

##[1,]  0.6666667 -0.1666667

##[2,] -0.3333333  0.3333333

##> my_matrix$set(matrix(c(2,3,5,1,3,7,4,5,6,8,0,0,4,5,6,0),4,4))

##> my_matrix$get()

##     [,1] [,2] [,3] [,4]

##[1,]    2    3    6    4

##[2,]    3    7    8    5

##[3,]    5    4    0    6

##[4,]    1    5    0    0

##> my_matrix$getinverse()

##NULL

##> cacheSolve(my_matrix)

##      [,1] [,2]       [,3]       [,4]

##[1,]  40.0  -30 -1.6666667  19.333333

##[2,]  -8.0    6  0.3333333  -3.666667

##[3,]   9.5   -7 -0.5000000   4.500000

##[4,] -28.0   21  1.3333333 -13.666667