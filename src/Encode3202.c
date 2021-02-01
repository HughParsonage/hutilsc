#include "hutilsc.h"

const int tens[9] = {1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000};

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

SEXP Encode3202(SEXP x) {
  R_xlen_t N = xlength(x);
  int typeofx = TYPEOF(x);
  if (typeofx != STRSXP) {
    error("x is not a character.");
  }
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int *restrict ansp = INTEGER(ans);
  
  for (R_xlen_t i = 0; i < N; ++i) {
    const char * xi = CHAR(STRING_ELT(x, i));
    size_t nchari = strlen(xi);
    bool starts_with_3202 = 
      nchari == 12 && (xi[0] == '3' && xi[1] == '2' && xi[2] == '0' && xi[3] == '2');
    /*    
     char *endptr;
     long long o = strtoll(xi, &endptr, 10);
     */
    int o = 0;
    if (starts_with_3202) {
      o = char12_to_int(xi);
    } else {
      o = char2int(xi, nchari);
    }
    
    ansp[i] = starts_with_3202 ? o : -o;
  }
  UNPROTECT(1);
  return ans;
}

int nth_digit_of(int x, int d) {
  x = (x < 0) ? -x : x;
  if (d < 0 || d > 8) {
    return 0;
  }
  int ten = tens[d];
  return (x / ten) % 10;
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

char* itoa2(int i, char b[]){
  char const digit[] = "0123456789";
  char* p = b;
  if(i<0){
    *p++ = '-';
    i *= -1;
  }
  int shifter = i;
  do{ //Move to where representation ends
    ++p;
    shifter = shifter/10;
  }while(shifter);
  *p = '\0';
  do{ //Move back, inserting digits as u go
    *--p = digit[i%10];
    i = i/10;
  }while(i);
  return b;
}

char* dig3202(int i, char b[]) {
  char const digit[] = "0123456789";
  char *p = b;
  if (i >= 0) {
    *p = '3';
    *p = '2';
    *p = '0';
    *p = '2';
  }
  i += (i <= 0) * 1e9;
  int shifter = i;
  do {
    ++p;
    shifter /= 10;
  } while (shifter);
  *p = '\0';
  do {
    *--p = digit[i % 10];
    i /= 10;
  } while (i);
  return b;
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

unsigned int baseTwoDigits(unsigned int x) {
  return x ? 32 - __builtin_clz(x) : 0;
}

static unsigned int baseTenDigits(unsigned int x) {
  static const unsigned char guess[33] = {
    0, 0, 0, 0, 1, 1, 1, 2, 2, 2,
    3, 3, 3, 3, 4, 4, 4, 5, 5, 5,
    6, 6, 6, 6, 7, 7, 7, 8, 8, 8,
    9, 9, 9
  };
  static const unsigned int tenToThe[] = {
    1, 10, 100, 1000, 10000, 100000, 
    1000000, 10000000, 100000000, 1000000000,
  };
  unsigned int digits = guess[baseTwoDigits(x)];
  return digits + (x >= tenToThe[digits]);
}

SEXP nDigits0(SEXP x) {
  R_xlen_t N = xlength(x);
  int typeofx = TYPEOF(x);
  if (typeofx != INTSXP) {
    error("x is not a integer.");
  }
  const int *xp = INTEGER(x);
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int *restrict ansp = INTEGER(ans);
  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] = n_digits0((unsigned int)xp[i]);
  }
  UNPROTECT(1);
  return ans;
}

SEXP nDigits1(SEXP x) {
  R_xlen_t N = xlength(x);
  int typeofx = TYPEOF(x);
  if (typeofx != INTSXP) {
    error("x is not a integer.");
  }
  const int *xp = INTEGER(x);
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int *restrict ansp = INTEGER(ans);
  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] = baseTenDigits((unsigned int)xp[i]);
  }
  UNPROTECT(1);
  return ans;
}


SEXP Digits(SEXP x) {
  R_xlen_t N = xlength(x);
  int typeofx = TYPEOF(x);
  if (typeofx != INTSXP) {
    error("x is not a integer.");
  }
  const int *xp = INTEGER(x);
  SEXP ans = PROTECT(allocVector(STRSXP, N));
  
  for (R_xlen_t i = 0; i < N; ++i) {
    int nd = xp[i] <= 0 ? n_digits0(xp[i] + 1e9) : 13;
    char digits11[nd];
    char *oip = dig3202(xp[i], digits11);
    const char *coip = oip;
    SET_STRING_ELT(ans, i, mkCharCE(coip, CE_UTF8));
  }
  UNPROTECT(1);
  return ans;
}


SEXP NthDigit(SEXP x, SEXP D) {
  R_xlen_t N = xlength(x);
  int typeofx = TYPEOF(x);
  if (typeofx != INTSXP) {
    error("x is not a integer.");
  }
  const int *xp = INTEGER(x);
  const int d = asInteger(D);
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int *restrict ansp = INTEGER(ans);
  
  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] = nth_digit_of(xp[i], d);
  }
  UNPROTECT(1);
  return ans;
}



static inline void reverse(char *upp, char *low)
{
  upp--;
  while (upp>low) {
    char tmp = *upp;
    *upp = *low;
    *low = tmp;
    upp--;
    low++;
  }
}

void writeInt32(int32_t *col, int64_t row, char **pch)
{
  char *ch = *pch;
  int32_t x = col[row];
  if (x == INT32_MIN) {
    *ch++ = 'N';
    *ch++ = 'A';
  } else {
    if (x<0) { *ch++ = '-'; x=-x; }
    // Avoid log() for speed. Write backwards then reverse when we know how long.
    char *low = ch;
    do { *ch++ = '0'+x%10; x/=10; } while (x>0);
    reverse(ch, low);
  }
  *pch = ch;
}

SEXP names2int(SEXP n1, SEXP n2) {
  R_xlen_t N = xlength(n1);
  R_xlen_t M = xlength(n2);
  if (N != M || TYPEOF(n1) != STRSXP || TYPEOF(n2) != STRSXP) {
    error("Internal error: N != M or non-character object");
  }
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int *restrict ansp = INTEGER(ans);
  
  for (R_xlen_t i = 0; i < N; ++i) {
    const char * xi = CHAR(STRING_ELT(n1, i));
    const char * yi = CHAR(STRING_ELT(n2, i));
    
    bool n0 = xi[0] != '\0' && xi[0] >= ' ';
    char c0 = n0 ? xi[0] : ' ';
    int  x0 = (int)(c0 - ' ');
    bool n1 = n0 && (xi[1] != '\0' && xi[1] >= ' ');
    char c1 = n1 ? xi[1] : ' ';
    int  x1 = (int)(c1 - ' ');
    
    bool n2 = yi[0] != '\0' && yi[0] >= ' ';
    char c2 = n2 ? yi[0] : ' ';
    int  x2 = (int)(c2 - ' ');
    bool n3 = n2 && (yi[1] != '\0' && yi[1] >= ' ');
    char c3 = n3 ? yi[1] : ' ';
    int  x3 = (int)(c3 - ' ');
    
    int oi = 0;
    int b = 1;
    oi += b * x0, b *= 91;
    oi += b * x1, b *= 91;
    oi += b * x2, b *= 91;
    oi += b * x3;
    ansp[i] = oi;
    
  }
  UNPROTECT(1);
  return ans;
}

SEXP lookup2_char(SEXP x) {
  R_xlen_t N = xlength(x);
  int typeofx = TYPEOF(x);
  if (typeofx != INTSXP) {
    error("x is not a integer.");
  }
  const int *xp = INTEGER(x);
  SEXP ans = PROTECT(allocVector(STRSXP, N));
  for (R_xlen_t i = 0; i < N; ++i) {
    int xpi = xp[i];
    if (!xpi) {
      SET_STRING_ELT(ans, i, mkChar(""));
      continue;
    }
    if (xpi < 0) {
      xpi = -xpi;
      char oi[2];
      oi[0] = ((char)xpi) + ' ';
      oi[1] = '\0';
      char *oip = oi;
      const char *coip = oip;
      SET_STRING_ELT(ans, i, mkChar(coip));
      continue;
    }
    char oc[3];
    oc[0] = ((char)(xpi % 91)) + ' ';
    oc[1] = ((char)((xpi % 8291) / 91)) + ' ';
    oc[2] = '\0';
    char *oip = oc;
    const char *coip = oip;
    SET_STRING_ELT(ans, i, mkCharCE(coip, CE_UTF8));
  }
  UNPROTECT(1);
  return ans;
}


SEXP lookup4_char(SEXP x) {
  R_xlen_t N = xlength(x);
  int typeofx = TYPEOF(x);
  if (typeofx != INTSXP) {
    error("x is not a integer.");
  }
  const int *xp = INTEGER(x);
  SEXP ans = PROTECT(allocVector(STRSXP, N));
  for (R_xlen_t i = 0; i < N; ++i) {
    int xpi = xp[i];
    if (!xpi) {
      SET_STRING_ELT(ans, i, mkChar(""));
      continue;
    }
    
    char oc[5];
    int b = 1;
    for (int c = 0; c < 4; ++c) {
      oc[c] = ((char)((xpi % (b * 91)) / b))  + ' ';
      b *= 91;
    }
    oc[4] = '\0';
    char *oip = oc;
    const char *coip = oip;
    SET_STRING_ELT(ans, i, mkCharCE(coip, CE_UTF8));
  }
  UNPROTECT(1);
  return ans;
}

SEXP pad0(SEXP x, SEXP width) {
  R_xlen_t N = xlength(x);
  const int w = asInteger(width);
  SEXP ans = PROTECT(allocVector(STRSXP, N));
  for (R_xlen_t i = 0; i < N; ++i) {
    const char * xi = CHAR(STRING_ELT(x, i));
    int strleni = strlen(xi);
    if (strleni >= w) {
      SET_STRING_ELT(ans, i, mkCharCE(xi, CE_UTF8));
      continue;
    }
    int z = w - strleni;
    char * acp = malloc(w * sizeof(char));
    for (int c = 0; c < z; ++c) {
      acp[c] = '0';
    }
    for (int c = z; c <= w; ++c) {
      acp[c] = xi[c - z];
    }
    const char * cacp = (const char *)acp;
    SET_STRING_ELT(ans, i, mkCharCE(cacp, CE_UTF8));
  }
  UNPROTECT(1);
  return ans;
}

SEXP tabulate_nchar18(SEXP x) {
  R_xlen_t N = xlength(x);
  if (TYPEOF(x) != STRSXP) {
    error("x is not a character.");
  }

  
  int counters[19][256] = {0};
  
  for (R_xlen_t i = 0; i < N; ++i) {
    const char * xi = CHAR(STRING_ELT(x, i));
    for (int c = 0; c < 19; ++c) {
      if (xi[c] == '\0') {
        break;
      }
      unsigned int xci = (unsigned int)(xi[c]);
      counters[c][xci] += 1;
    }
  }
  SEXP ans = PROTECT(allocVector(INTSXP, 19));
  int *restrict ansp = INTEGER(ans);
  
  for (int i = 0; i < 19; ++i) {
    int n = 0;
    for (int j = 0; j < 256; ++j) {
      n += counters[i][j] > 0;
    }
    ansp[i] = n;
  }
  UNPROTECT(1);
  return ans;
}

SEXP encodeRecordID(SEXP x) {
  R_xlen_t N = xlength(x);
  if (TYPEOF(x) != STRSXP) {
    error("x is not a character.");
  }
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int *restrict ansp = INTEGER(ans);
  
  for (R_xlen_t i = 0; i < N; ++i) {
    //const char * xi = CHAR(STRING_ELT(x, i));
    // 5002e0000052JlLAAU
    // 012345678901234567
    int a = 0;
    ansp[i] = a;
    
  }
  UNPROTECT(1);
  return ans;
  
}


