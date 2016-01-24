## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#makecacheMatrix creats a special matrix, which is really a list contating a functon to
#set the value of the matirx
#get the value of the matirx
#set the value of the inverse(inv)
#get the value of the inverse(inv)

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){
                x<<-y
                inv<-NULL
        }
        get<-function() x
        setinv<-function(inv_m) inv<<-inv_m
        getinv<-function() inv
        list(set=set , get=get , getinv=getinv , setinv=setinv)
}

## Write a short comment describing this function
#The following function calculates the inverse of the matrix created with the above function.
#It first checks to see if the mean has already been caculated.
#If so, it gets the inverse from the cache and skips the computation.
#Otherwise, it calcultaes the inverse of the data and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x) {
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}

        ## Return a matrix that is the inverse of 'x'
        
