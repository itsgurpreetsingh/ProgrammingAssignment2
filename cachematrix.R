## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#Defined a function setin that sets matrix to the matrix which is passed as parameter
#setinverse function sets inverse to the passed argument
#getin function returns matrix 
#getinverse function returns inverse
makeCacheMatrix <- function(x = matrix()) {
  invers.e<-NULL      #inverse is initially set to null
  setin<-function(z)
  {
    x<<-z
    invers.e<<-NULL
  }
  getin<-function() x
  setinverse<-function(inverse) invers.e<<-inverse
  getinverse<-function()invers.e
  list(setin=setin,getin=getin,setinverse=setinverse,getinverse=getinverse)
}

## Write a short comment describing this function

#this function gets inverse from x
#it checks if inverse is not null then it means it was cached
#if inverse is null then it calculate inverse and set to invers.e

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
 invers.e<-x$getinverse()
 if(!is.null(invers.e))
 {
   return(invers.e)
 }
 matrixx<-x$getin()
 invers.e<-solve(matrixx)
 x$setinverse(invers.e)
 invers.e                     ## Return a matrix that is the inverse of 'x'
}
