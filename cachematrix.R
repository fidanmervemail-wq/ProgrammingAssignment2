> makeCacheMatrix <- function(x = matrix()) {
     s <- NULL
     set <- function(y){
         x <<- y
         s <<- NULL
     }
     get <- function()x
     setInverse <- function(inverse) s <<- inverse
     getInverse <- function() s 
     list(set = set, get = get, 
          setInverse = setInverse, 
          getInverse = getInverse)
 }
 cacheSolve <- function(x, ...) {
     s <- x$getInverse()
     if(!is.null(s)){
         message("getting cached data")
         return(s)
     }
     mat <- x$get()
     s <- solve(mat,...)
     x$setInverse(s)
     s
 }
