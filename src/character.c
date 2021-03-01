#include "hutilsc.h"

int tens[10] = {1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000};

int char12_to_int(const char * x) {
  int o = 0;
  int ten = 1;
  for (int i = 11; i >= 4; --i) {
    o += ten * (x[i] - '0');
    ten *= 10;
  }
  return o;
}

int char2int(const char * x, int s) {
  int o = 0;
  int ten = 1;
  for (int i = s - 1; i >= 0; --i) {
    o += ten * (x[i] - '0');
    ten *= 10;
  }
  return o;
}

bool all_digits_4_12(const char * xi) {
  for (int j = 4; j < 12; ++j) {
    char xj = xi[j];
    if (xj < '0' || xj > '9') {
      return false;
    }
  }
  return true;
}

bool all_digits(const char * xi, size_t nchari) {
  for (size_t j = 0; j < nchari; ++j) {
    if (xi[j] < '0' || xi[j] > '9') {
      return false;
    }
  }
  return true;
}

int ipow10(int n) {
  unsigned int j = n % 10U; 
  return tens[j];
}

int n_digits0(unsigned int x) {
  if (x >= 1000000000U) return 10;
  if (x >= 100000000U)  return 9;
  if (x >= 10000000U)   return 8;
  if (x >= 1000000U)    return 7;
  if (x >= 100000U)     return 6;
  if (x >= 10000U)      return 5;
  if (x >= 1000U)       return 4;
  if (x >= 100U)        return 3;
  if (x >= 10U)         return 2;
  return 1;
} 

int nth_digit_of(int x, int n) {
  if (n >= 10) {
    return (x / 1000000000);
  }
  if (n) {
    int M = ipow10(n);
    return (x % M) / (M / 10);
  } else {
    return (x % 10);
  }
}

char digit2char(int d) {
  switch(d) {
  case 0: 
    return '0';
  case 1: 
    return '1';
  case 2: 
    return '2';
  case 3: 
    return '3';
  case 4: 
    return '4';
  case 5: 
    return '5';
  case 6: 
    return '6';
  case 7: 
    return '7';
  case 8: 
    return '8';
  case 9: 
    return '9';
  }
  return '0';
}

unsigned char nth_char(int x, int n) {
  // 123456 <- ans
  // 012345 <- d
  int d = nth_digit_of(x, n);
  return digit2char(d);
}


SEXP do_nth_digit_of(SEXP x, SEXP n) {
  if (TYPEOF(x) != INTSXP || TYPEOF(n) != INTSXP) {
    return R_NilValue;
  }
  R_xlen_t N = xlength(x);
  if (N > 1 && xlength(n) != N && xlength(n) != 1) {
    return R_NilValue;
  }
  const int * xp = INTEGER(x);
  
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  
  for (R_xlen_t i = 0; i < N; ++i) {
    int xi = xp[i];
    int ni = (xlength(n) == 1) ? asInteger(n) : INTEGER(n)[i];
    if (ni > 10 || xi < 0) {
      ansp[i] = NA_INTEGER;
      continue;
    }
    ansp[i] = nth_digit_of(xi, ni);
  }
  UNPROTECT(1);
  return ans;
}

SEXP do_nth_char_of(SEXP x, SEXP n) {
  if (TYPEOF(x) != INTSXP || TYPEOF(n) != INTSXP || xlength(n) != 1) {
    return R_NilValue;
  }
  R_xlen_t N = xlength(x);
  int nn = asInteger(n);
  
  const int * xp = INTEGER(x);
  SEXP ans = PROTECT(allocVector(STRSXP, N));
  
  for (R_xlen_t i = 0; i < N; ++i) {
    unsigned char uci = nth_char(xp[i], nn);
    char ci[2] = {uci, '\0'};
    const char * cci = (const char *)ci;
    SET_STRING_ELT(ans, i, mkCharCE(cci, CE_UTF8));
  }
  UNPROTECT(1);
  return ans;
}
