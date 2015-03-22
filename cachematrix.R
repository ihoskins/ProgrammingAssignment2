## Put comments here that give an overall description of what your
## functions do

# The makeCacheMatrix and cacheSolve functions are meant to interplay:
# list elements of the first function are used in the second
# (to supply a cached evaluation or update the input matrix), 
# and code in the second function assigns new computed values 
# to the cache, the framework of which is the first function's list. 
# For this to work, lexical scoping must be at play so that
# recomputed values set to the list are updated in the cache. 
# 

## Write a short comment describing this function
# makeCacheMatrix is a function that returns a list of 
# four functions for getting and setting cached values
# of a matrix inverse operation using the cacheSolve function.

makeCacheMatrix<-function(current_matrix=matrix()) {
# Initialize the object that will cache the desired value
  cached_inv<-NULL
# Create function for reassigning an updated matrix to "current_matrix"; reassign cached inverse concurrently
# (The "<<-" operater searches the parent environments for the value of "current_matrix" and reassigns it;
# if it doesn't find it, it creates the object in the global env.)
  set_data<-function(updated_matrix) {
    current_matrix<<-updated_matrix
    cached_inv<<-NULL
  }
# Create function for retrieving the current matrix in the other function
  get_data<-function() current_matrix
# Create function for setting the evaluation of the matrix inverse:
# A newly computed value will be reassigned to cached_inv in the parent env of this function (line #4)
  set_m_inv<-function(inv) cached_inv<<-inv
# Create function for retrieving the evaluation of cached_inv
  get_m_inv<-function() cached_inv
# Create list with all four functions as elements
# (This is the return value for the makeCacheMatrix function and will be assigned
# to a variable for use with the other function)
  list(set_data=set_data, get_data=get_data, set_m_inv=set_m_inv, get_m_inv=get_m_inv)
}


## Write a short comment describing this function
# cacheSolve is a function that either retrieves a cached value
# of the matrix inverse operation or recalculates it by retrieiving
# the updated input data from the makeCacheMatrix's list.

cacheSolve<-function(current_matrix, ...) {
# Assign to inv the cached value of the inverse by subsetting from special list "current matrix"
  inv<-current_matrix$get_m_inv()
# If inv is not null, print message and return the cached evaluation (the return function executes its argument last)
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
# If not cached, assign the cached matrix to "data" for computation
  data <- current_matrix$get_data()
# Calculate the inverse of the matrix
  inv <- solve(data)
# Then cache the computed inverse
  current_matrix$set_m_inv(inv)
  inv
}

