// dist_func.cpp
#include <Rcpp.h>

#define a 6378137.0
#define m_pi 3.141592653589793238462643383280

// convert degrees to radians
// [[Rcpp::export]]
double deg_to_rad_(double degree) {
  return(degree * m_pi / 180.0);
}

// compute Haversine distance between two points
// [[Rcpp::export]]
double dist_haversine_(double xlon,
		       double xlat,
		       double ylon,
		       double ylat) {


  // return 0 if same point
  if (xlon == ylon && xlat == xlon) return 0.0;

  // convert degrees to radians
  xlon = deg_to_rad_(xlon);
  xlat = deg_to_rad_(xlat);
  ylon = deg_to_rad_(ylon);
  ylat = deg_to_rad_(ylat);

  // haversine distance formula
  double d1 = sin((ylat - xlat) / 2.0);
  double d2 = sin((ylon - xlon) / 2.0);

  return 2.0 * a * asin(sqrt(d1 * d1 + cos(xlat) * cos(ylat) * d2 * d2));

}

// compute many to many distances and return matrix
// [[Rcpp::export]]
Rcpp::NumericMatrix dist_mtom_(Rcpp::NumericVector xlonv,
			       Rcpp::NumericVector xlatv,
			       Rcpp::NumericVector ylonv,
			       Rcpp::NumericVector ylatv,
			       Rcpp::CharacterVector x_names,
			       Rcpp::CharacterVector y_names) {

  // init output matrix (x X y)
  int n = xlonv.size();
  int k = ylonv.size();
  Rcpp::NumericMatrix distmat(n,k);

  // double loop through each set of points to get all combinations
  for(int i = 0; i < n; i++) {
    for(int j = 0; j < k; j++) {
      distmat(i,j) = dist_haversine_(xlonv[i], xlatv[i], ylonv[j], ylatv[j]);
    }
  }

  // add row and column names
  rownames(distmat) = x_names;
  colnames(distmat) = y_names;
  return distmat;
}

// compute and return minimum distance along with name
// [[Rcpp::export]]
Rcpp::DataFrame dist_min_(Rcpp::NumericVector xlonv,
			  Rcpp::NumericVector xlatv,
			  Rcpp::NumericVector ylonv,
			  Rcpp::NumericVector ylatv,
			  Rcpp::CharacterVector x_names,
			  Rcpp::CharacterVector y_names) {

  // init output matrix (x X 3)
  int n = xlonv.size();
  int k = ylonv.size();
  Rcpp::CharacterVector minvec_name(n);
  Rcpp::NumericVector minvec_meter(n);
  Rcpp::NumericVector tmp(k);

  // loop through each set of starting points
  for(int i = 0; i < n; i++) {
    for(int j = 0; j < k; j++) {
      tmp[j] = dist_haversine_(xlonv[i], xlatv[i], ylonv[j], ylatv[j]);
    }

    // add to output matrix
    minvec_name[i] = y_names[which_min(tmp)];
    minvec_meter[i] = min(tmp);
  }

  // return created data frame
  return Rcpp::DataFrame::create(Rcpp::Named("fips11") = x_names,
				 Rcpp::Named("unitid") = minvec_name,
				 Rcpp::Named("meters") = minvec_meter,
				 Rcpp::_["stringsAsFactors"] = false);
}


