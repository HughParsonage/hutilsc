#include "hutilsc.h"

SEXP Crorder(SEXP x, SEXP o, SEXP nthreads, SEXP InPlace) {
	R_xlen_t N = xlength(x);
	if (TYPEOF(x) != INTSXP || TYPEOF(o) != INTSXP || xlength(o) != N) {
		error("Types or lengths wrong."); // # nocov
	}
	const int * xp = INTEGER(x);
	const int * op = INTEGER(o);

	int nThread = asInteger(nthreads);
	const bool in_place = asLogical(InPlace); 
	
	
	
	SEXP ans = PROTECT(allocVector(INTSXP, N));
	int * ansp = INTEGER(ans); 

#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
for (R_xlen_t i = 0; i < N; ++i) {
	ansp[i] = xp[op[i] - 1];
}
	UNPROTECT(1);
	return ans;
}
