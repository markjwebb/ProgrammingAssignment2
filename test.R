source("cacheMean.R")
source("cachematrix.R")

testCacheMean <- function() {
    a = c(1,2,3)
    mean_a = mean(a)
    b = makeVector(a)
    calculated = cachemean(b)
    if(calculated == mean_a) {
        print(sprintf("Calculated mean is correct: %f",calculated))
    }
    else {
        print(sprintf("Calculated mean: Expected=%f, Actual=%f",mean_a,calculated))
    }
    cached = cachemean(b)
    if(cached == mean_a) {
        print(sprintf("Cached mean is correct: %f",cached))
    }
    else {
        print(sprintf("cached mean: Expected=%f, Actual=%f",mean_a,cached))
    }
}

testCacheSolve <- function() {
    data = c(8,1,6,3,5,7,4,9,2)
    m = matrix(data,3,3)
    inverse = solve(m)
    
    b = makeCacheMatrix(m)
    calculated = cacheSolve(b)
    if(identical(calculated,inverse)) {
        print("Calculated inverse is correct")
    }
    else {
        print(sprintf("Calculated inverse: Expected=%s, Actual=%s",str(inverse),str(calculated)))
    }
    cached = cacheSolve(b)
    if(identical(cached,inverse)) {
        print("Cached inverse is correct")
    }
    else {
        print(sprintf("cached inverse: Expected=%s, Actual=%s",str(inverse),str(cached)))
    }
}