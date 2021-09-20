# The functions 'makeCacheMatrix' and 'cacheSolve' will demonstrate the lexical scoping of R 
#
#    "Lexical scoping is used to retrieve values from objects based on the way functions are nested when they were written"
# 
# A cache is a way to store objects in memory to accelerate subsequent access to the same object. 
# In statistics, some matrix algebra computations are notoriously expensive, such as calculating the inverse of a matrix.
# Therefore, if one needs to use the same inverted matrix for subsequent computations, it is advantageous to cache it in memory 
# instead of repeatedly calculating the inverse. 
# 

makeCacheMatrix <- function(x = matrix()) {
  #This function creates a special "matrix" object that can cache its inverse.
  inv<-NULL
  
  #setter
  set<-function(y){
    x<<-y # assign value to object in an environment different to the current
    inv<<-NULL
  }
  setinv<-function(inverse) inv<<-inverse
  
  #getter
  get<-function() x
  getinv<-function() inv
  
  #returning list of object
  list(set=set,
       get=get,
       setinv=setinv,
       getinv=getinv)
}

cacheSolve <- function(x, ...) {
  # Returns the inverse of the matrix passed to 'makeCacheMatrix'
  # in addition, the inverse matrix is cached for future use
  
  inv<-x$getinv()
  
  # check if we already have the inverse
  if(!is.null(inv)){
    message("Using cached data")
    return(inv)
  }
  data<-x$get()
  invData<-solve(data)
  x$setinv(invData)
  return(invData)
}

testAssign2<- function(){
  # This can be used to test makeCacheMatrix and cacheSolve
  # and is based on https://www.coursera.org/learn/r-programming/discussions/weeks/3/threads/ePlO1eMdEeahzg7_4P4Vvg
  
  m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
  myMatrix_object <- makeCacheMatrix(m1)
  
  # cacheSolve(myMatrix_object) should return 
  #         [,1] [,2]
  #    [1,]    6    8
  #    [2,]    2    4
  #
  # in addition we need to time the executition time as calling cacheSolve the first time should take longer than any 
  # subsequent call with the same argument
  start.time = Sys.time()
  res1<-cacheSolve(myMatrix_object)
  dur1 = Sys.time() - start.time
  print(dur1)
  print(res1)
  
  start.time = Sys.time()
  res2<-cacheSolve(myMatrix_object)
  dur2 = Sys.time() - start.time
  print(dur2)
  print(res2)
  
  print(dur2-dur1)
  
}
