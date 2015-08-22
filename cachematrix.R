
#Function is used to set and get input matrix/inverse
#Similar to Bean class in java
makeCacheMatrix <- function(z=matrix()){
  
  #function takes z matrix input
  #returns a list with functions to
  #set get input/inverse matrix
  
  invrs <- NULL
  
  #In this function we are setting input matrix using <<- operator
  set <- function(p){
    # <<- is a special operator to assign value and cache/store it
    z <<- p
    invrs <<- NULL
    
  }
  #function to get input matrix
  get<- function() z
  #function to set inverse of matrix
  setinverse <- function(inversed) invrs <<- inversed
  #Function to get inverse of matrix
  getinverse <- function() invrs
  #return list
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
  
}

#This function is used to get inverse of matrix if already set
#If the inverse is null calculating it and setting it in makecachematrix
cacheSolve <- function(z,...){
  
  #trying to get inverse matrix if its already set
  invrs <- z$getinverse()
  
  #finding if fetched inverse matrix is null or not
  #if not null returning
  if(!is.null(invrs)){
    
    message("getting cached data")
    return(invrs)
    
  }
  
  #finding the inverse of matrix
  data <- z$get()
  invrs <- solve(data,...)
  
  #setting the calculated inverse
  z$setinverse(invrs)
  return(invrs)
  
}


