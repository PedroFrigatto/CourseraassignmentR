#these two functions are capable of storing matrices and its corresponding inverses

#this first function needs a matrix as an input, and must be assigned to an 
#object, for example:x<-matrix(1:4,2,2) ; ex<-makecachematrix(x)
#every time we type ex$get(), we will get the input matrix
#to get the inverse we should type x$getInv(), if the result is NULL, then
#we need to use the second function

makeCacheMatrix <- function(x=matrix()){
        invert<-NULL
        set<-function(y){
                x<<-y
                invert<<-NULL
        }
        get<- function(){x}
        setInv<- function(inverse) {invert<<-inverse}
        getInv <- function() {invert}
        list(set=set,get=get,setInv=setInv,getInv=getInv)
        
}

#to get the inverse of the matrix computed, we should simply use a function
#already computed in the makeCacheMatrix as an argument to this function
#following the example, we should do: cacheSolve(ex)
#after doing this, we should be able to get the matrix inverse by
#typing: ex$getInv()
cacheSolve<- function(x,...){
        invert<-x$getInv()
        if(!is.null(invert)){
                print("getting data from cache")
                return(invert)
        }
        mat <- x$get()
        invert <- solve(mat,...)
        x$setInv(invert)
        invert
}