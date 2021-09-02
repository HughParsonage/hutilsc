#include "hutilsc.h"

SEXP unique_sorted_int(const int * x, R_xlen_t N) {
	if (N <= 1) {
		return ScalarInteger(N);
	}
	int o = 1;
	for (R_xlen_t i = 1; i < N; ++i) {
		o += x[i - 1] != x[i];
	}
	return ScalarLength(o);
}

SEXP CkuniqueN(SEXP x) {
	switch(TYPEOF(x)) {
		case INTSXP:
		return unique_sorted_int(INTEGER(x), xlength(x));

	}
	return R_NilValue;
}
