
## functions do

## defining the four functions : set,get,setinverse and getinverse
## Also making a list of these four functions inside the function called 
## makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
  
  mat_inv<-NULL
  
  #assiging the user defined matric to variable y 
  
  set <- function(y){
    x<<-y
    mat_inv<<-NULL
  }
  
  #Reading the matrix stored in x
  
  get<-function() x
  
  #Storing the calculated inverse of matrix in variable mat_inv 
  
  setinverse <- function(inverse) mat_inv<<-inverse
  
  #Returning already stored inverse matrix in varaible mat_inv to the called 
  #function
  
  getinverse<- function() mat_inv
  
  #Defining a list containing the functions defined above
  list (set=set, get=get,
        setinverse=setinverse, getinverse=getinverse)
  
}


## this functions checks if the inverse of the matrix is calculated and already 
# stored in cache. If cached it returns it value otherwise it goes ahead and 
# calculates the inverse of the matrix, caches it and then returns it as output 

cacheSolve <- function(x, ...) {
  
  mat_inv <- x$getinverse() # calling getinverse to check cache value
  
  if(!is.null(mat_inv)){
    message("Reading from Cache")
    return(mat_inv)
  }
  
  mat <- x$get()
  
  mat_inv <- solve(mat,...)
  
  x$setinverse(mat_inv)
  
  mat_inv
  ## Return a matrix that is the inverse of 'x'
}