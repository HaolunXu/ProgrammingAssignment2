Mat <- matrix(c(8,2,7,4,3,6,9,5,1),nrow=3,ncol=3)

makeCacheMatrix <- function(x=matrix()){
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInv <- function(solve) m <<- solve
	getInv <- function() m
	list(set=set,get=get,
		setInv=setInv,
		getInv=getInv)
}

M <- makeCacheMatrix(Mat)

cacheSolve <- function(x,...){
	m <- x$getInv()
	if(!is.null(m)){
		message("get cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data,...)
	x$setInv(m)
	m
}

InvMat <- cacheSolve(M)
InvMat
