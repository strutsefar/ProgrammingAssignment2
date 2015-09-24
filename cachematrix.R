
#makeInv
#This function takes a matrix as an argument and holds several subfunctions that allows us to store data
#The function generates and stores a list of subfunctions that can be accessed and used by other functions by subsetting the main function
#The solution follows the stucture of the original assignment


makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL #Initialize the inv variable to a NULL Value. This value will eventually store the inverted matrix. 
  #We will later use the NULL value to determine if the matrix has allready been calculated or not
  
  set <- function(y) { #Function to change the matrix stored in the main function (x), with another value (y). Not really needed for the assignment but included to match the structure of the example 
    x <<- y            #The value x is accessible outside the main function because we use the <<- operator
    inv <<- NULL       #If we actually use this function to change the matrix we need to reset the stored inverted result as it is no longer valid 
  }
  
  get <-  function() x    # Function that returns the matrix stored in the variable x in the main function
  
  setinv <- function(solve) inv <<- solve #Sets the inv variable to the value of solve. (NB. The actual solving of the matrix does not happen here but is passed to the function via the solve variable)
  getinv <- function() inv
  
  list(set = set, get = get, #Store the subfunctions in a list so that they are available by subsetting the makeInv function 
       setinv = setinv,
       getinv = getinv)
}


#This function checks to see if we allready have calculated the result
#If that is the case we simply return the calculated result
#If it is not allready done we calculate it and store it for future use

cacheSolve <- function(x, ...) {
  inv <- x$getinv()   #Get the stored value of the inv variable
  
  if(!is.null(inv)) { #Check to see if result is allready calculated (inv is not NULL). 
    
    message("Allready calculated, getting cached result")
    return(inv)            #If the result was allready calculated then we don't do it again but rather return the result
  }
  data <- x$get()          #If the result is not allready calculated we retieve the matrix (x) stored in the get function. Assign it to a variable called data
  inv <- solve(data, ...)  #Solve invert the matrix called data
  x$setinv(inv)            #Store the result for future use
  inv
}
