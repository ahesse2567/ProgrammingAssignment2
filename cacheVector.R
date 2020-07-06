# this is the example provided. I've inserted my comments that document my understanding of the functions

makeVector <- function(x = numeric()) {
    m <- NULL # m is for the MEAN of the vector. The mean IS NOT SET UNTIL YOU USE THE GETMEAN FUNCTION WITHIN THE LIST
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean # this is NOT the mean function (i.e. mean()) but rather the mean value calculated from within the cachemean function. It's just confusing that m represents the mean and we're setting the mean to the mean, but we're not using the mean function THIS TIME.
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}
