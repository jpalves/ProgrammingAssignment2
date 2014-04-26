##this set of functions can store the calculation of the inverse matrix
##so that it can be used in computation involving repetitions, 
##the computational cost of inverting a huge matrix even parallel computing is high
#makeCacheMatrix -> object to create the cache and stores the inverse
#methods:
#
#set 	-> set the matrix x
#get 	-> return the matrix x
#setinv -> stores the inverse of matrix x
#getinv -> return the inverse of matrix x
#
#cacheSolve is a function with makeCacheMatrix object input and return the inverse
#
#example code
#X <- makeCacheMatrix()
#A<-rbind(c(1,3),c(5,6))
#X$set(A)
#A_1 <- cacheSolve(X)
#A_2 <- cacheSolve(X)


##caches inverse matrix for not to be calculated more than one time

makeCacheMatrix <- function(x = matrix()) {
	I   <- NULL
	set <- function(m){
		x <<- m
		I <<-NULL
	}
	get    <- function() x
	setinv <- function(inv) I <<- inv
	getinv <- function() I
	list(set    = set, 
	     get    = get,
		 setinv = setinv, 
		 getinv = getinv)
}

## retrives inverse matrix from cache if exists if not calulate with solve

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		I <- x$getinv()
		if(!is.null(I)){
			message("getting cached data")
			return(I)
		}
		data <- x$get()
		I<-solve(data,...)
		x$setinv(I)
		return(I)
}
