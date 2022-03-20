#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

// [[Rcpp::export]]
double distance_radian_coordinates(double latf, double lonf, 
                               double latt, double lont) {
  
  double dlat = latt - latf;
  double dlon =  lont - lonf;
  
  double a = pow(sin(dlat/2),2) + cos(latf) * cos(latt) * pow(sin(dlon/2),2);
  double c = 2*atan2(sqrt(a), sqrt(1-a));
  
  double distance = 6378137.0 *c;
  return distance;
}

// [[Rcpp::export]]
Rcpp::NumericMatrix interacting_pairs(int i, DatetimeVector datetime, 
                                      NumericVector latitude, NumericVector longitude, int temporal_thresh, int spatial_thresh) {
  int n = datetime.size();
  double diff;
  double tmp_distance;
  NumericVector distances;
  IntegerVector js;
  Function distance_radian_coordinates( "distance_radian_coordinates") ;
  for(int j = i+1; j <= (n-1); j++){
    diff = datetime[j] - datetime[i];
    if((diff/60) > temporal_thresh) break;
    
    else{
      
      tmp_distance = as<double>(distance_radian_coordinates(latitude[i], longitude[i], latitude[j], longitude[j]));
      if(tmp_distance < spatial_thresh){
        js.push_back(j+1);
        distances.push_back(tmp_distance);
      }
    }
  }
  NumericMatrix mat(2, js.size());
  mat.row(0) = js;
  mat.row(1) = distances;
  return mat;
}