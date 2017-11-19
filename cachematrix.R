## The overall objective of these functions is to accept a matrix from the user and calculate its inverse in an efficient manner
## Matrix inversions can be costly calculations.
## The objective is to cache the inverse of the matrix for future usage in order to save the computational time.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## THis functions helps in creating a matrix type object which stores two variables - the matrix and its inverse. 
## The values of matrix and its inverse can be accessed/modified externally by the functions get,set, getinv,setinv
makeCacheMatrix <- function(x = matrix()) {
  invx<-NULL
  set<-function(y=matrix()){
    x<<-y
    invx<<-NULL
  }
  get<-function() x
  setinv<-function(invtemp){
    invx<<-invtemp
  }
  getinv<-function() invx
  list(set=set,get=get,setinv=setinv,getinv=getinv)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated(and the matrix has not changed), then 
## cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invX1<-x$getinv()
  if(!is.null(invX1)){
    message("getting cached data")
    return(invX1)
  }
  X1<-x$get()
  invX1<-solve(X1)
  x$setinv(invX1)
  invX1
  
}
## Once we make a user given matrix as a makeCacheMatrix object, 
## We can access the user matrix using the makeCacheMatrix object functions such as set,get,setinv,getinv
## How it works in the console
## m1<-matrix(c(1/2,-1/4,-1,3/4),nrow=2,ncol=2)
## myMatrixObj<-makeCacheMatrix(m1)
## inv1<-cacheSolve(m1)
## m1*inv1 should give an identity matrix



