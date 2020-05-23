## This script creates a "matrix" that will store as cache the inverse of the
## matrix input as a parameter

## This function creates the R object that keeps the cache data
makeCacheMatrix<- function (x = matrix()) {
        I<- NULL
        set<- function(y){
                x<<-y
                I<<-NULL
        }
        get<-function () x
        setinverse<-function(inverse) I<<-inverse
        getinverse<-function() I
        list(set=set, get=get, setinverse=setinverse, getinverse = getinverse)
}

## This function checks if the given object has cached data. If it does, it 
##returns the cached data. Otherwise, it calculates the inverse and stores it 
##in cache
cacheSolve<-function(x,...){
        I<-x$getinverse()
        if(!is.null(I)){
                message("getting cached data")
                return(I)
        }
        data<-x$get()
        I<-solve(data,...)
        x$setinverse(I)
        message("data cached successfully")
        I
}
