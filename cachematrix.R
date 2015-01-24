# This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix<-function(x=matrix){      
  m<-NULL               # Define an empty matrix
  
  set<-function(y){     # Set the value of the matrix
    x<<-y
    m<<-NULL
  }
  
  get<-function() x                         # Get the value of the matrix
  setinv<-function(inverse) m<<-inverse     # Set the value of the inverse
  getinv<-function() m                      # Get the value of the inverse
  
  list(set=set, get=get,                    # define the list of all the subfunctions
       setinv=setinv,
       getinv=getinv)
}

# This function computes the inverse of the special 
# "matrix" returned by makeCacheMatrix above. If the inverse has already 
# been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x=matrix(), ...) {
  m <- x$getinv()           # recall "subfunction" getinv on matrix x (to which I've assigned the MakeCacheMatrix function)
  
  if(!is.null(m)) {         # check if the inverse has already been calculated
    message("getting cached data")   # if yes, it returns this message... and then the old value
    return(m)
  }
  # otherwise
  data <- x$get()           # it take the new inputed matrix...
  m <- solve(data, ...)     # and assigns it to the m object
  x$setinv(m)               # and sets the value othe inverse
  m                         # print m
}
