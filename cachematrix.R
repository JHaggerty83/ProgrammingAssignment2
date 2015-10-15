## These functions take a matrix as an input, uses the solve() function to return the inverse matrix and caches the inverse matrix for future use. For large matrixes in which the inverse is required multiple times, it is quicker to recall the matrix from the cached value rather than run a solve() function on the matrix each time.

##-----makeCacheMatrix

## The makeCacheMatrix takes one input (a matrix) and returns a list with four named elements (each of the elements is the value of another function). The set function uses the special assign character to assign values to x and m globablly within these function.

##The get function takes no input and returns the value of x (the input)

##The setSolve funtion takes one input (the inverse matrix), and assigns that matrix to variable m globally within these functions.

## The getInverse function takes no inputs and returns m.

##The list elements return the value assigned when called by name with the '$" character.


makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set<-function(y){
		x<<-y
		m<<-NULL
	}
	get<-function()x
	setSolve<-function(Inverse_Matrix) m<<-Inverse_Matrix
	getInverse<-function() m
	list(set=set, get=get, setSolve=setSolve, getInverse=getInverse)
}


## Write a short comment describing this function
##-------cacheSolve

##This function takes and input matrix and uses the solve() function to return the inverse.

## The first line calls on the elements of the makeCacheMatrix (specifically the getInverse element) and assigns it locally to variable m

##The if block checks to see if the value of m is null. If it is not null the if statement continues, prints a message alerting the user that the cached data is being fetched, then returns the stored value of m (the previously inverted matrix)

##If m is null then the if statement is skipped, the value of the makeCacheMatrix list element 'get' is assigned locally to variable 'data'

##The matrix in variable data is inverted and assigned locally to m

##Variable m is passed to the setSolve function and assigned globally within these functions to m

##The value assigned to m is returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getInverse()
        if(!is.null(m)){
        	message("getting cached data") 
        	return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setSolve(m)
        m
}
