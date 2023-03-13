// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// distance_radian_coordinates
double distance_radian_coordinates(double latf, double lonf, double latt, double lont);
RcppExport SEXP _aniSNA_distance_radian_coordinates(SEXP latfSEXP, SEXP lonfSEXP, SEXP lattSEXP, SEXP lontSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type latf(latfSEXP);
    Rcpp::traits::input_parameter< double >::type lonf(lonfSEXP);
    Rcpp::traits::input_parameter< double >::type latt(lattSEXP);
    Rcpp::traits::input_parameter< double >::type lont(lontSEXP);
    rcpp_result_gen = Rcpp::wrap(distance_radian_coordinates(latf, lonf, latt, lont));
    return rcpp_result_gen;
END_RCPP
}
// interacting_pairs
Rcpp::NumericMatrix interacting_pairs(int i, DatetimeVector datetime, NumericVector latitude, NumericVector longitude, int temporal_thresh, int spatial_thresh);
RcppExport SEXP _aniSNA_interacting_pairs(SEXP iSEXP, SEXP datetimeSEXP, SEXP latitudeSEXP, SEXP longitudeSEXP, SEXP temporal_threshSEXP, SEXP spatial_threshSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type i(iSEXP);
    Rcpp::traits::input_parameter< DatetimeVector >::type datetime(datetimeSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type latitude(latitudeSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type longitude(longitudeSEXP);
    Rcpp::traits::input_parameter< int >::type temporal_thresh(temporal_threshSEXP);
    Rcpp::traits::input_parameter< int >::type spatial_thresh(spatial_threshSEXP);
    rcpp_result_gen = Rcpp::wrap(interacting_pairs(i, datetime, latitude, longitude, temporal_thresh, spatial_thresh));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_aniSNA_distance_radian_coordinates", (DL_FUNC) &_aniSNA_distance_radian_coordinates, 4},
    {"_aniSNA_interacting_pairs", (DL_FUNC) &_aniSNA_interacting_pairs, 6},
    {NULL, NULL, 0}
};

RcppExport void R_init_aniSNA(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
