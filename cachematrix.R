## This fuction is for matrix inversion. The first fxn
## creates a list sets the parameters, and the second
## function creates checks if the matrix has already
## been caclucalted and then returns either the 
## cacluaton or the cached calculation. 

## This first fuction creates a list that:
## 1.  sets the value of the vector
## 2.  gets the value of the vector
## 3.  sets the value of the mean
## 4.  gets the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  LeInverse <- NULL
  set <- function(y) {
    x <<- y
    LeInverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) LeInverse <<- inverse
  getinverse <- function() LeInverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This fxn returns the inverse. First it checks
## to see if the inverse in cahched, and if it is 
## it returns it, if not it caluclates it and 
## returns it.

cacheSolve <- function(x, ...) {
  LeInverse <- x$getinverse()
   if(!is.null(LeInverse)) {
    message("getting cached inverse")
    return(LeInverse)
  }
  data <- x$get()
  LeInverse <- solve(data)
  x$setinverse(LeInverse)
  LeInverse
}
  
## Return a matrix that is the inverse of 'x'
SampleMX <- matrix(1:4,2,2)
MX = makeCacheMatrix(SampleMX)
cacheSolve(MX) ##first itt
cacheSolve(MX) ##second itt with message
