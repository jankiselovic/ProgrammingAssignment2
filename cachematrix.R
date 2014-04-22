## makeCacheMatrix function create matrix that can cache its inverse
## cacheSolve function can calculate inverse of matrix from makeCacheMatrix
## in case that inverse has been already calculated, it can retrieve from the cache

## makeCacheMatrix create a matix, which is list containing below function
  #setvalue of matrix
  #get value of matrix
  #set inverse of matrix
  #get inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y){
        x<<-y
        inv<<-NULL
    }
    get<-function()x
    setinverse<-function(inverse)inv<<-inverse
    getinverse<-function()inv
    list(set=set,get=get,
          setinverse=setinverse,
          getinverse=getinverse)
}


## cacheSolve calculates inverse of matrix from makeCacheMatrix, in case inverse has been calculated
## function can retrieve it from cache 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  if(!is.null(inv)){
      message("getting cached data")
      return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x<-setinverse(inv)
  inv
}
