#include "hutilsc.h"


double sinhalfsq (double x) {
  const double o = sin(x / 2.0);
  return o * o;
}

double haversine_dist(double olat1, double olon1, double olat2, double olon2) {
  const double lat1 = olat1 * (M_PI / 180) ;
  const double lat2 = olat2 * (M_PI / 180) ;
  const double lon1 = olon1 * (M_PI / 180) ;
  const double lon2 = olon2 * (M_PI / 180) ;
  
  
  const double delta_lat = (lat1 > lat2) ? (lat1 - lat2) : (lat2 - lat1) ;
  //const double delta_lat = std::fabs(lat1 - lat2);
  const double delta_lon = (lon1 > lon2) ? (lon1 - lon2) : (lon2 - lon1) ;
  // const double delta_lon = std::fabs(lon1 - lon2);
  
  // 6371 * 2 * asin(sqrt(sin(d_lat / 2)^2 + cos(lat1) * cos(lat2) * sin(d_lon / 2)^2))
  double out = 0;
  double den = cos(lat1) * cos(lat2) * sinhalfsq(delta_lon);
  out = sinhalfsq(delta_lat);
  out += den;
  out = sqrt(out);
  out = asin(out);
  out *= 6371;
  out *= 2;
  return out;
}

double fhaversine_dist(double olat1, double olon1, double olat2, double olon2, bool allow_float) {
  const double lat1 = olat1 * (M_PI / 180) ;
  const double lat2 = olat2 * (M_PI / 180) ;
  const double lon1 = olon1 * (M_PI / 180) ;
  const double lon2 = olon2 * (M_PI / 180) ;
  
  
  //const double delta_lat = (lat1 > lat2) ? (lat1 - lat2) : (lat2 - lat1) ;
  const double delta_lat = fabs(lat1 - lat2);
  //const double delta_lon = (lon1 > lon2) ? (lon1 - lon2) : (lon2 - lon1) ;
  const double delta_lon = fabs(lon1 - lon2);
  
  // 6371 * 2 * asin(sqrt(sin(d_lat / 2)^2 + cos(lat1) * cos(lat2) * sin(d_lon / 2)^2))
  float out = 0;
  float den = cos(lat1) * cos(lat2) * sinhalfsq(delta_lon);
  out = sinhalfsq(delta_lat);
  out += den;
  out = ssqrt_fast(out);
  out = asin(out);
  out *= 6371;
  out *= 2;
  return (double)out;
}

SEXP Chaversine_distance(SEXP olat1, SEXP olon1, 
                           SEXP olat2, SEXP olon2,
                           SEXP allow_sqrt_float) {
  R_xlen_t N = xlength(olat1);
  
  // # nocov start
  if (N != xlength(olon1) ||
      N != xlength(olat2) ||
      N != xlength(olon2)) {
    error("lengths differ");
  }
  if (!isReal(olat1) || !isReal(olon1) ||
      !isReal(olat2) || !isReal(olon2)) {
      error("Not all real.");
  }
  // # nocov end
  
  const bool allow_float = asLogical(allow_sqrt_float);
  
  const double * olat1p = REAL(olat1);
  const double * olon1p = REAL(olon1);
  const double * olat2p = REAL(olat2);
  const double * olon2p = REAL(olon2);
  
  SEXP ans = PROTECT(allocVector(REALSXP, N));
  double *restrict ansp = REAL(ans);
  
  if (allow_float) {
    for (R_xlen_t i = 0; i < N; ++i) {
      ansp[i] = fhaversine_dist(olat1p[i], olon1p[i],
                                olat2p[i], olon2p[i], true);
    }
  } else {
    for (R_xlen_t i = 0; i < N; ++i) {
      ansp[i] = haversine_dist(olat1p[i], olon1p[i],
                               olat2p[i], olon2p[i]);
    }
  }
  UNPROTECT(1);
  return ans;
}





