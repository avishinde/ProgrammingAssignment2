## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        get<-function() x
        setMatrixInv<-function(inverse) inv<<-inverse
        getMatrixInv<-function() inv
        list(set = set, get = get,
             setMatrix = setMatrix,
             getMatrix = getMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getMatrixInv()
        if(!is.null(inv)){
                message("getting cashed data")
                return(inv)
        }
        mat<-x$get()
        inv<- solve(mat,...)
        x$setMatrixInv(inv)
        inv
  }
