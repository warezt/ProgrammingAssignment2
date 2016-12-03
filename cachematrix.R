## Put comments here that give an overall description of what your
## functions do
## This function will put x vector into list of four function
## set the value of the vector
## get the value of the vector
## set the value of the inverse (m)
## get the value of the inverse (m)
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL          ##Create null variable
  set <- function(y) {   ##create set function. It will get y variable from global environment and put it back to x
    x <<- y              ##set y into x variable in global environment
    m <<- NULL         ##while setting, also create m variable as a null in global environment
  }
  get <- function() x     ##just retrieve x from main function
  setinverse <- function(inverse) m <<- inverse   ##setinverse will input inverse into m variable
  getinverse <- function() m                      ##just retrieve m variable and display it 
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Write a short comment describing this function
## This function will check whether there is any inversed informaiton readily provided. If there is already an information, it will just get information from that cache.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()     ##put getinverse in  x list (the m variable) into m function
  if(!is.null(m)) {       ##Check if m has info. If it has info, get info from m then it is done
    message("getting cached data.")
    return(m)
  }
  data <- x$get()           ##if there is no info in m, getinformation from get in x list whose take information from x vector and write it as data variable
  m <- solve(data)        ## once data is settled, cal inverse of data
  x$setinverse(m)         ## input m variable into m variable.
  m
        ## Return a matrix that is the inverse of 'x'
}
