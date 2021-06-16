#include "hutilsc.h"
#include <xmmintrin.h>

inline void SSESqrt_Recip_Times_X(float * restrict pOut, float * restrict pIn )
{
  __m128 ink = _mm_load_ss(pIn);
  _mm_store_ss(pOut, _mm_mul_ss(ink, _mm_rsqrt_ss(ink)));
  // compiles to movss, movaps, rsqrtss, mulss, movss
}

SEXP showsqrt_fast(SEXP x) {
  // printout the compiler-time context
  
#ifndef _MSC_VER //Optimal
  #ifdef __AVX__
     Rprintf("AVX-");
  #else
      Rprintf("NOT-AVX-");
  #endif
    
  Rprintf("B");
#else //TODO: not optimal when in AVX mode or when not inlined
  Rprintf("C");
#endif
    return R_NilValue;
}

inline float rsqrt_fast(float x) {
  /* https://stackoverflow.com/questions/32687079/getting-fewest-instructions-for-rsqrtss-wrapper */
#ifndef _MSC_VER //Optimal
  float result;
  asm( //Note AT&T order
#ifdef __AVX__
    "vrsqrtss %1, %1, %0"
#else
    "rsqrtss %1, %0"
#endif
    : "=x"(result)
    : "x"(x)
  );
  return result;
#else //TODO: not optimal when in AVX mode or when not inlined
  return _mm_cvtss_f32(_mm_rsqrt_ss(_mm_set_ps1(x)));
#endif
}


float ssqrt_fast(float x) {
  /* https://stackoverflow.com/questions/32687079/getting-fewest-instructions-for-rsqrtss-wrapper */
#ifndef _MSC_VER //Optimal
  float result;
  asm( //Note AT&T order
#ifdef __AVX__
    "vsqrtss %1, %1, %0"
#else
    "sqrtss %1, %0"
#endif
    : "=x"(result)
    : "x"(x)
  );
  return result;
#else //TODO: not optimal when in AVX mode or when not inlined
  return _mm_cvtss_f32(_mm_sqrt_ss(_mm_set_ps1(x)));
#endif
}

double euclid_dist_d(double d0, double d1) {
  float squared_dist = d0 * d0 + d1 * d1;
  return (double)ssqrt_fast(squared_dist); 
}

double euclid_dist(double x0, double y0, double x1, double y1) {
  double d0 = y0 - x0;
  double d1 = y1 - x1;
  float squared_dist = d0 * d0 + d1 * d1;
  return (double)ssqrt_fast(squared_dist);
}


SEXP Csqrt2(SEXP x) {
  R_xlen_t N = xlength(x);
  if (!isReal(x)) {
    return R_NilValue;
  }
  const double *xp = REAL(x);
  SEXP ans = PROTECT(allocVector(REALSXP, N));
  double * restrict ansp = REAL(ans);
  for (R_xlen_t i = 0; i < N; ++i) {
    float xpi = xp[i];
    // float oi = xpi * rsqrt_fast(xpi);
    float oi = ssqrt_fast(xpi);
    ansp[i] = oi;
  }
  UNPROTECT(1);
  return ans;
}

SEXP euclid(SEXP x1, SEXP y1, SEXP x2, SEXP y2, SEXP uu) {
  R_xlen_t N = xlength(x1);
  if (xlength(y1) != N || xlength(x2) != N || xlength(y2) != N) {
    error("Lengths differ.");
  }
  const double *x1p = REAL(x1);
  const double *y1p = REAL(y1);
  const double *x2p = REAL(x2);
  const double *y2p = REAL(y2);
  
  const bool use_sqrt = asLogical(uu);
  
  SEXP ans = PROTECT(allocVector(REALSXP, N));
  double * restrict ansp = REAL(ans);
  for (R_xlen_t i = 0; i < N; ++i) {
    double xpi = x2p[i] - x1p[i];
    double ypi = y2p[i] - y1p[i];
    ansp[i] = xpi * xpi + ypi * ypi;
    if  (use_sqrt) {
      ansp[i] = sqrt(ansp[i]);
    }
  }
  
  UNPROTECT(1);
  return ans;
}


