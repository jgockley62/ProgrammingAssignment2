#Here's a test matrix to test the code:
TEST<-rbind(c(3,0,2),c(2,0,-2),c(0,1,1))
TEST2<-rbind(c(3,0,1),c(2,0,-2),c(0,1,1))

##makeCacheMatrix takes a matrix and returns a list of funtions
#Example Call: foo<-makeCacheMatrix(TEST)
#Result:
#1-foo$get: stores a function foo$get() - which returns the input matrix when foo$get() is called 
#2-foo$getsolve: stores a function foo$getsolve() - which returns the inverse of the input matrix
#3-foo$set: returns a null variable
#4-foo$setsolve: function to call solve on variable m (which is a null variable to be replaced later if necesary)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve is a function to examine a makeCacheMatrix list object (example foo from the above test code)
#Searches the object from makeCacheMatrix for the stored inverse matrix. If it exists it is returned.
#If it isn't stored it is calulated and returned
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
