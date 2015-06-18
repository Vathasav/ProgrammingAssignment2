## The first function, makeCacheMatrix creates a special "matrix" object that can cache its inverse
## It creates a list containing a function to set the value of matrix, get the value of matrix


makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
      set<-function(y){
        x<<-y
        m<<-NULL
      }
    get<-function() x
    setmatrix<-function(mat) m<<- mat
    getmatrix<-function() m
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}


## his function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m<-x$getmatrix()
      if(!is.null(m)){
        message("getting cached data")
        return(m)
      }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}
