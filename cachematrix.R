## Put comments here that give an overall description of what your
## functions do

## there are two functions
##makeCaacheMatrix consists of set,get,setinv,getinv
##library(MASS) is used to calculate inverse for non-squared as well as squared matix

library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL     #initializing inverse as null
  set<-function(y){
                  x<<-y
                  inv<<-NULL
                  }
  
  get<-function()x      #function to obtain inverse
  setinv<-function(inverse)inv<<-inverse
  getinv<-function(){
                    inver<-ginv(x)
                    inver%*%x     #function to obtain inverse
                    }
  
  list(set = set,get = get, setinv = setinv, getinv = getinv)
  
}


## Write a short comment describing this function
##this is used to get cache data

cacheSolve <- function(x, ...) ##gets cache data 
  {
  inv<-x$getinv()
  if(!is.null(inv)){
                    message("getting cached data!")
                    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...) #calculates inverse
  x$setinv(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
