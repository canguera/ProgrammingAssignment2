## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix())
{
##begins by setting the solve to NULL as a placeholder for a future value
  i<-NULL 
  
  ##defines a function to set the matrix, x, to a new matrix, y, and resets
  ##the solve, i, to NULL
  set<-function(y)
  {
    x<<-y
    i<<-NULL
  }
##returns the matrix, x
  get<-function()x
  
  ##sets the solve, i, to solve
  setinversa<-function(solve)i<<-solve
  ##returns the inverse, i
  getinversa<-function()i
  
  ##returns the 'special matrix' containing all of the functions just defined
  list(set=set,get=get,setinversa=setinversa,getinversa=getinversa)


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
         ##get the matrix
  i<-x$getinversa()
  ##If the inverse has already been calculated, or no change
 
  if(!is.null(i))
  {
    ##then the cachesolve should retrieve call cache
    message("getting cached data")
    return(i)
  }
  ##get value matrix
  data<-x$get()
  ## calculated the inverse matrix
  i<-solve(data,...)
  ##call function setinversa returns value matrix inverse
  x$setinversa(i)
  i
}
