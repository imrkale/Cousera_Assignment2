## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a special matrix object that can cache its inverse.
##It will set the matrix value, get the matrix value, set the inverse matrix 
##value and get that value

makeCacheMatrix <- function(x = matrix()) {
invmat<-NULL  		##Will contain the inverse matrix value
set<-function(y)
{
x<<-y
invmat<<-NULL		##set inversematrix to NULL
}
get<-function() x

setinverse<-function(inverse)invmat<<-inverse  ##set the inverse matrix
getinverse<-function()invmat  ##get the inverse matrix
list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function
## This function takes the output of the previous matrix as an input and checks
##inverse matrix from makeCachematric(matrix) has any value in it or not.
##In case inverse matrix from makeCachematrix(matrix) is empty, it gets the original
##matrix data and set the inverse matrix by using the solve function
 
cacheSolve <- function(x, ...) {
invmat<-x$getinverse()
if(!is.null(invmat))
{
	message("getting cached data")
	return(invmat)
}
data<-x$get()
invmat<-solve(data,...)
x$setinverse(invmat)
invmat
        ## Return a matrix that is the inverse of 'x'
}
