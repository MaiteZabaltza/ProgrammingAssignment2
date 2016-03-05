## This is my submission for the R Programming Assessment 2.
## The following two functions have been created in order to cache the inverse of a matrix.
## An inverteble matrix supply is assumed for these funtions. 

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      cache<-NULL
      setmatrix<-function(y){
            x<<-y
            cache<-NULL
      }
      getmatrix<- function()x
      setsolve<-function(solve) cache<<-solve
      getsolve<-function()cache
      list(setmatrix=setmatrix, getmatrix=getmatrix, setsolve=setsolve, getsolve=getsolve)
}

## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        cache<-x$getsolve()
        if(!is.null(cache)){
              message("getting cached data")
              return(cache)
        }
        dat<-x$getmatrix()
        cache<-solve(dat)
        x$setsolve(cache)
        cache
}