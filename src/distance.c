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

SEXP do_haversine_distance(SEXP olat1, SEXP olon1, 
                           SEXP olat2, SEXP olon2,
                           SEXP allow_sqrt_float) {
  R_xlen_t N = xlength(olat1);
  if (N != xlength(olon1) ||
      N != xlength(olat2) ||
      N != xlength(olon2)) {
    error("lengths differ");
  }
  if (!isReal(olat1) || !isReal(olon1) ||
      !isReal(olat2) || !isReal(olon2)) {
      error("Not all real.");
  }
  
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

SEXP one_edge_dist(SEXP xs, SEXP ys, SEXP k1, SEXP k2) {
  const int x = asInteger(xs);
  const int y = asInteger(ys);
  if (TYPEOF(k1) != INTSXP || TYPEOF(k2) != INTSXP ||
      xlength(k1) != xlength(k2) || xlength(k1) > INT_MAX) {
    return R_NilValue;
  }
  
  int N = length(k1);
  const int * K1 = INTEGER(k1);
  const int * K2 = INTEGER(k2);
  
  int d = 0;
  
  // position of x within K1
  int xloc = radix_find(K1, x, 0, N, N);
  if (xloc < 0 || xloc >= N || K1[xloc] != x) {
    // maybe in other edge list
    xloc = linear_find(K2, x, N);
    if (xloc == N) {
      // not found in either edge column --> does not appear as an edge
      d = INT_MAX;
      SEXP ans = PROTECT(allocVector(INTSXP, 1));
      INTEGER(ans)[0] = d;
      UNPROTECT(1);
      return ans;
    }
  }
  // position of x within K1
  int yloc = radix_find(K1, y, 0, N, N);
  if (yloc < 0 || yloc >= N || K1[yloc] != y) {
    // maybe in other edge list
    yloc = linear_find(K2, y, N);
    if (yloc == N) {
      d = INT_MAX;
      SEXP ans = PROTECT(allocVector(INTSXP, 1));
      INTEGER(ans)[0] = d;
      UNPROTECT(1);
      return ans;
    }
  }
  
  
  // distance = 1
  int j = xloc;
  while (j < N) {
    if (K2[j] == y) {
      d = 1;
      SEXP ans = PROTECT(allocVector(INTSXP, 1));
      INTEGER(ans)[0] = d;
      UNPROTECT(1);
      return ans;
    }
    if (K1[j] != x) {
      break;
    }
    ++j;
  }
  
  int n_down = j;
  
  // distance = 2
  for (j = xloc; j < n_down; ++j) {
    
    // for each edge directly, look at the indirects
    int nj = K2[j];
    int jj = radix_find(K1, nj, 0, N, N);
    
    
    while (jj < N && K1[jj] == nj) {
      Rprintf("%d %d %d", jj, nj);
      if (K2[jj] == y) {
        // distance 2
        d = 2;
        SEXP ans = PROTECT(allocVector(INTSXP, 1));
        INTEGER(ans)[0] = d;
        UNPROTECT(1);
        return ans;
      }
      ++jj;
    }
    
  }
  
  
  
  SEXP ans = PROTECT(allocVector(INTSXP, 1));
  INTEGER(ans)[0] = INT_MAX;
  UNPROTECT(1);
  return ans;
}



