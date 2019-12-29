# following function creates the special matrix and caches the inverse of the matrix.
# for the function, x and y is the normal matrices
# value can be obtained with the code {invmat$getCacheInvMatrix()}
# and value can be change with the code {invmat$setCacheInvMatrix()}
# return list containing four functions to set and get the value of the matrix and to set and get the inverse of the matrix

CacheInvMatrix <- function(x = matrix()){
  invmat <- NULL
  set <- function(y){
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  setCacheInvMatrix <- function(solve)invmat <<- solve
  getCacheInvMatrix <- function()invmat
  list(set = set, get = get, 
       setCacheInvMatrix = setCacheInvMatrix,
       getCacheInvMatrix = getCacheInvMatrix)
}

# below function computes the inverse of the special matrix "CacheInvMatrix"
#If for the further calcultion, matrix has not changed, then the cachesolvemat retrieves the inverse from the cache


cachesolvemat <- function(x, ...){
  invmat <- x$getCacheInvMatrix() # for retriving the cached value while computing inverse
  if(!is.null(invmat)){ # if the cache was not empty, then it print the following message and return the function.
    message("getting cached data")
    return(invmat)
  }
  # if the cache was empty, below codes calculate the inverse and cache it and return it.
  data <- x$get() #get the value
  invmat <- solve(data, ...) # calculate inverse
  x$setCacheInvMatrix(invmat) # cache the result
  invmat # return the inverse
}
