#include "hutilsc.h"

SEXP qgraph_layout_Cpp(SEXP Pniter,
                       SEXP Pvcount,
                       SEXP Pecount,
                       SEXP Maxdelta,
                       SEXP Parea,
                       SEXP Pcoolexp,
                       SEXP Prepulserad,
                       SEXP EEf,
                       SEXP EEt, 
                       SEXP WW,
                       SEXP xxInit,
                       SEXP yyInit,
                       SEXP Cxx,
                       SEXP Cyy) {
  /*
   Calculate a two-dimensional Fruchterman-Reingold layout for (symmetrized) 
   edgelist matrix d.  Positions (stored in (x,y)) should be initialized
   prior to calling this routine.
   */
  if (notInt(Pniter) || notInt(Pvcount) || notInt(Pecount)) {
    error("Expected int. (%d)", notInt(Pniter) + 2 * notInt(Pvcount) + 4 * notInt(Pecount)); // # nocov
  }
  int pniter = asInteger(Pniter);
  int pvcount = asInteger(Pvcount);
  int pecount = asInteger(Pecount);
  int n = pvcount;
  
  if (TYPEOF(Maxdelta) != REALSXP || xlength(Maxdelta) != n) {
    error("Maxdelta not REAL or xlength(Maxdelta) != n"); // # nocov
  }
  const double * maxdelta = REAL(Maxdelta);
  if (notDbl(Parea) || notDbl(Pcoolexp) || notDbl(Prepulserad)) {
    error("Expected dbl."); // # nocov
  }
  double parea = asReal(Parea);
  double pcoolexp = asReal(Pcoolexp);
  double prepulserad = asReal(Prepulserad);
  
  if (notEquiInt2(EEf, EEt)) {
    error("notEquiInt2(EEf, EEt)");
  }
  const int * Ef = INTEGER(EEf);   // Edges from 
  const int * Et = INTEGER(EEt);  // Edges t0
  
  const double * W = REAL(WW);
  if (TYPEOF(WW) != REALSXP || xlength(WW) != xlength(EEf)) {
    error("WW not REAL or equilength with EEf."); // # nocov
  }
  
  if (notEquiDbl2(xxInit, yyInit)) {
    error("notEquiDbl2(xxInit, yyInit)");
  }
  const double * xInit = REAL(xxInit);
  const double * yInit = REAL(yyInit);
  if (notEquiLgl2(Cxx, Cyy)) {
    error("notEquiLgl2(Cxx, Cyy)");
  }
  const int * Cx = LOGICAL(Cxx);
  const int * Cy = LOGICAL(Cyy);
  
  
  int m = pecount;
  double frk;
  double ded;
  double xd;
  double yd;
  double rf;
  double af;
  int i;
  int j;
  int k;
  int l;
  int niter = pniter;
  //double maxdelta;
  double area = parea;
  double coolexp = pcoolexp;
  double repulserad = prepulserad;
  
  // Unprotections
  int np = 0;
  SEXP dxx = PROTECT(allocVector(REALSXP, n)); np++;
  SEXP dyy = PROTECT(allocVector(REALSXP, n)); np++;
  SEXP tt = PROTECT(allocVector(REALSXP, n)); np++;
  double * dx = REAL(dxx);
  double * dy = REAL(dyy);
  double * t = REAL(tt);
  
  // Copy xIint and yInit:
  SEXP xx = PROTECT(allocVector(REALSXP, n)); np++;
  double * x = REAL(xx);
  SEXP yy = PROTECT(allocVector(REALSXP, n)); np++;
  double * y = REAL(yy);
  
  for (int i = 0; i < n; i++) {
    x[i] = xInit[i];
    y[i] = yInit[i];
  }
  
  frk = sqrt(area/(double)n);
  double frksq = area/((double)n);
  
  SEXP pows = PROTECT(allocVector(REALSXP, niter)); np++;
  double * powsp = REAL(pows);
  for (int i = 0; i < niter; ++i) {
    powsp[i] = pow((double)i / (double)niter, coolexp);
  }
  
  // Run the annealing loop
  for (i = niter; i >= 0; i--) {
    // Clear the deltas
    for (j = 0; j < n; j++){
      dx[j] = 0.0;
      dy[j] = 0.0;
    }
    // Increment deltas for each undirected pair
    for (j = 0; j < n; j++) {
      // Set the temperature (maximum move/iteration)
      t[j] = maxdelta[j] * powsp[i];
      
      for (k = j + 1; k < n; k++){
        // Obtain difference vector
        xd = x[j] - x[k];
        yd = y[j] - y[k];
        float dedsq = xd * xd + yd * yd;
        ded = ssqrt_fast(dedsq);  // Get dyadic euclidean distance
        xd /= ded;                // Rescale differences to length 1
        yd /= ded;
        // Calculate repulsive "force"
        rf = frksq * (1.0/ded - dedsq/repulserad);
        xd *= rf;
        yd *= rf;
        dx[j] += xd;        // Add to the position change vector
        dx[k] -= xd;
        dy[j] += yd;
        dy[k] -= yd;
      }
      
    }
    // Calculate the attractive "force"
    for (j = 0; j < m; j++) {
      k = Ef[j] - 1;
      l = Et[j] - 1;
      
      xd = x[k] - x[l];
      yd = y[k] - y[l];
      ded = euclid_dist_d(xd, yd);
      // ded = sqrt(xd*xd + yd*yd);  // Get dyadic euclidean distance
      if (ded > 0.000001 || ded < -0.000001) {
        xd /= ded;                // Rescale differences to length 1
        yd /= ded;
      }
      af = ded * ded / frk * W[j];
      dx[k] -= xd * af;        // Add to the position change vector
      dx[l] += xd * af;
      dy[k] -= yd * af;
      dy[l] += yd * af;
    }
    // Dampen motion, if needed, and move the points
    for (j = 0; j < n; j++) {
      // ded = sqrt(dx[j]*dx[j] + dy[j]*dy[j]);
      ded = euclid_dist_d(dx[j], dy[j]);
      if (ded > t[j]) {                 // Dampen to t
        ded = t[j] / ded;
        dx[j] *= ded;
        dy[j] *= ded;
      }
      if (!Cx[j]) {
        x[j] += round(dx[j] * 1e5) / 1e5; // Update positions (correcting for floating point errors)
      }
      if (!Cy[j]) {
        y[j] += round(dy[j] * 1e5) / 1e5;
      }
    }
  }
  SEXP ans = PROTECT(allocVector(VECSXP, 2)); np++;
  SET_VECTOR_ELT(ans, 0, xx);
  SET_VECTOR_ELT(ans, 1, yy);
  UNPROTECT(np);
  return ans;
}
