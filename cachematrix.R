## set the input x to be a matrix
## set the solved value "z" to be null
## changed any references to "mean" to "solve"

#MakeCacheMatrix#

>makeCacheMatrix <- function(x = matrix()) {
     z <- NULL
     set <- function(y){
         x <<- y
         z <<- NULL
     }
     get <- function()x
     setInverse <- function(inverse) z <<- inverse
     getInverse <- function() z 
     list(set = set, get = get, 
          setInverse = setInverse, 
          getInverse = getInverse)
 }


##
## changed "mean" to "solve" and "z" again

#CacheSolve#

 cacheSolve <- function(x, ...) {
     z <- x$getInverse()
     if(!is.null(z)){
         message("getting cached data")
         return(z)
     }
     mat <- x$get()
     z <- solve(mat,...)
     x$setInverse(z)
     z
 }

