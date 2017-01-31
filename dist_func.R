## dist_func.R

## convert degrees to radians
deg_to_rad <- function(degree) {
    m_pi <- 3.141592653589793238462643383280
    return(degree * m_pi / 180)
}

## compute Haversine distance between two points
dist_haversine <- function(xlon, xlat, ylon, ylat) {

    ## radius of Earth in meters
    a <- 6378137

    ## return 0 if same point
    if (xlon == ylon & xlat == xlon) { return(0) }

    ## convert degrees to radians
    xlon = deg_to_rad(xlon)
    xlat = deg_to_rad(xlat)
    ylon = deg_to_rad(ylon)
    ylat = deg_to_rad(ylat)

    ## haversine distance formula
    d1 <- sin((ylat - xlat) / 2)
    d2 <- sin((ylon - xlon) / 2)

    return(2 * a * asin(sqrt(d1^2 + cos(xlat) * cos(ylat) * d2^2)))

}

## compute many to many distances and return matrix
dist_mtom <- function(xlonv,         # vector of starting longitudes
                      xlatv,         # vector of starting latitudes
                      ylonv,         # vector of ending longitudes
                      ylatv,         # vector of ending latitudes
                      x_names,       # vector of starting point names
                      y_names) {     # vector of ending point names

    ## init output matrix (n X k)
    n <- length(xlonv)
    k <- length(ylonv)
    mat <- matrix(NA, n, k)

    ## double loop through each set of points to get all combinations
    for(i in 1:n) {
        for(j in 1:k) {
            mat[i,j] <- dist_haversine(xlonv[i], xlatv[i], ylonv[j], ylatv[j])
        }
    }

    ## add row and column names
    rownames(mat) <- x_names
    colnames(mat) <- y_names
    return(mat)
}

## compute and return minimum distance along with name
dist_min <- function(xlonv,         # vector of starting longitudes
                     xlatv,         # vector of starting latitudes
                     ylonv,         # vector of ending longitudes
                     ylatv,         # vector of ending latitudes
                     x_names,       # vector of starting point names
                     y_names) {     # vector of ending point names

    ## NB: lengths: x coords == x names && y coords == y_names
    n <- length(xlonv)
    k <- length(ylonv)
    minvec_name <- vector('character', n)
    minvec_meter <- vector('numeric', n)

    ## init temporary vector for distances between one x and all ys
    tmp <- vector('numeric', k)

    ## give tmp names of y vector
    names(tmp) <- y_names

    ## loop through each set of starting points
    for(i in 1:n) {
        for(j in 1:k) {
            tmp[j] <- dist_haversine(xlonv[i], xlatv[i], ylonv[j], ylatv[j])
        }

        ## add to output matrix
        minvec_name[i] <- names(which.min(tmp))
        minvec_meter[i] <- min(tmp)
    }

    return(data.frame('fips11' = x_names,
                      'unitid' = minvec_name,
                      'meters' = minvec_meter,
                      stringsAsFactors = FALSE))
}


