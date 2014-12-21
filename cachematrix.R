# Overall Description - makeCacheMatrix() is used to make a special "Cache Matrix" which is 
# basically a list which holds a matrix, a place for the matrix's inverse and a couple getter
# and setter functions. cacheSolve() is the function called on a "Cache Matrix" which solves
# the inverse of the matrix and then stores it in the matrix's place for the inverse function
# cacheSolve() only solves and stores the inverse if the inverse hasn't already been stored
# in the Cache Matrix.


# for a description of makeCacheMatrix look above
makeCacheMatrix <- function(x = matrix()) { # input x will be a matrix
    i <- NULL # i is the inverse that is set to null every time makeCacheMatrix is called...
    
    # making the getters and setters for cacheSolve()
    
    get <- function() {x} # this method returns the value of the matrix 
                          # passed to makeCacheMatrix
    
    setInverse <- function(solve) {i <<- solve} # this is called by cacheSolve() first
                # It will save the solved (inverted) matrix using super assignment  to i
  
    getInverse <- function() {i} # return the cached inverse when asked
    
    list(get = get, setInverse = setInverse, getInverse = getInverse) # this list makes the
                                  # nested functions available for use through the console.
}


# For a description of this function see Overall description above makeCacheMatrix()
cacheSolve <- function(x, ...) { # the input x is an object created by makeCacheMatrix
                                 # It returns a matrix that is the inverse of 'x'
  
  m <- x$getInverse() # get the inverse from x if it already has it cached
  
  if(!is.null(m)) {       # if the inverse isn't null (i.e. is cached)
      message("getting cached data") # send this message to the console
      return(m)               # return the cached inverse, ending this function
  }
  
  data <- x$get()       #the code down here will only happen if m is not NULL (not cached)
  m <-solve(data,...)   #calculate the inverse if not cached
  x$setInverse(m)       #store the calculated inverse in x 
  m                     #return the inverse
  
}