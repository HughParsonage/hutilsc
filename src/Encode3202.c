#include "hutilsc.h"




SEXP CValidate3202(SEXP x) {
  R_xlen_t N = xlength(x);
  int typeofx = TYPEOF(x);
  if (typeofx != STRSXP) {
    error("x is not a character.");
  }
  for (R_xlen_t i = 0; i < N; ++i) {
    if (STRING_ELT(x, i) == NA_STRING) {
      continue;
    }
    const char * xi = CHAR(STRING_ELT(x, i));
    size_t nchari = strlen(xi);
    if (nchari > 10) {
      if (nchari != 12) {
        error("PHESSID contains nchar(x[i]) > 10");
      }
      bool starts_with_3202 = 
        (xi[0] == '3' && xi[1] == '2' && xi[2] == '0' && xi[3] >= '0' && xi[3] <= '9');
      if (!starts_with_3202 || !all_digits_4_12(xi)) {
        error("PHESSID contains nchar(xi) == 10 but not starting with 3202 or otherwise not digit.");
      }
    } else {
      if (!all_digits(xi, nchari)) {
        error("PHESSID contains non-digits");
      }
    }
  }
  return ScalarLogical(1);
}

SEXP CEncode3202(SEXP x) {
  R_xlen_t N = xlength(x);
  if (TYPEOF(x) != STRSXP) {
    error("x is not a character.");
  }
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int *restrict ansp = INTEGER(ans);
  
  for (R_xlen_t i = 0; i < N; ++i) {
    if (STRING_ELT(x, i) == NA_STRING) {
      ansp[i] = NA_INTEGER;
      continue;
    }
    const char * xi = CHAR(STRING_ELT(x, i));
    size_t nchari = strlen(xi);
    bool starts_with_3202 = false;
    if (nchari > 10) {
      if (nchari != 12) {
        error("PHESSID contains nchar(x[i]) > 10");
      }
      starts_with_3202 = 
        (xi[0] == '3' && xi[1] == '2' && xi[2] == '0' && xi[3] >= '0' && xi[3] <= '9');
      if (!starts_with_3202 || !all_digits_4_12(xi)) {
        error("PHESSID contains nchar(xi) == 10 but not starting with 3202 or otherwise not digit.");
      }
    } else {
      if (!all_digits(xi, nchari)) {
        error("PHESSID contains non-digits");
      }
    }
    int o = 0;
    if (starts_with_3202) {
      o = char12_to_int(xi);
    } else {
      o = char2int(xi, nchari);
    }
    // for not starting with '3202' we still want
    // to preserve the order so we subtract a large 
    // number from it.  We have established that
    // every number not starting with 3202 is no 
    // larger than 1e9 (10 digits) and is nonnegative.
    ansp[i] = starts_with_3202 ? o : (o - 1e9);
  }
  UNPROTECT(1);
  return ans;
}

char* dig3202(int i, char b[]) {
  char const digit[] = "0123456789";
  char *p = b;
  bool pad0 = i >= 0;
  if (pad0) {
    *p++ = '3';
    *p++ = '2';
    *p++ = '0';
    *p++ = '2';
    for (int z = 4; z < 12; ++z) {
      *p++ = '0';
    }
  } else {
    i += (i <= 0) * 1e9;
    int shifter = i;
    do {
      ++p;
      shifter /= 10;
    } while (shifter);
  }
  *p = '\0';
  do {
    *--p = digit[i % 10];
    i /= 10;
  } while (i);
  return b;
}


SEXP CDecode3202(SEXP x) {
  R_xlen_t N = xlength(x);
  int typeofx = TYPEOF(x);
  if (typeofx != INTSXP) {
    error("x is not a integer.");
  }
  const int *xp = INTEGER(x);
  SEXP ans = PROTECT(allocVector(STRSXP, N));
  
  for (R_xlen_t i = 0; i < N; ++i) {
    if (xp[i] == NA_INTEGER) {
      SET_STRING_ELT(ans, i, NA_STRING);
      continue;
    }
    int nd = xp[i] <= 0 ? n_digits0(xp[i] + 1e9) : 13;
    char digits11[nd];
    char *oip = dig3202(xp[i], digits11);
    const char *coip = oip;
    SET_STRING_ELT(ans, i, mkCharCE(coip, CE_UTF8));
  }
  UNPROTECT(1);
  return ans;
}



/*
 * Now encode first two characters of each namee
 */

SEXP Cnames2int(SEXP n1, SEXP n2) {
  R_xlen_t N = xlength(n1);
  R_xlen_t M = xlength(n2);
  if (N != M || TYPEOF(n1) != STRSXP || TYPEOF(n2) != STRSXP) {
    error("Internal error: N != M or non-character object");
  }
  // For 4 characters, we have exactly enough room
  // with a 32 bit int.  I just used 91 to project
  // other representations.
  int len_alphabet = ((int)'z') - ((int)' ');
  if (len_alphabet != 90) {
    error("Internal error: len_alphabet != 90");
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

bool is_normal_RecordID(const char * x) {
  bool o = 
    x[0] == '5' &&
    x[1] == '0' &&
    x[2] == '0' &&
    x[3] == '2' &&
    x[4] == 'P' &&
    x[5] == '0' &&
    x[6] == '0' &&
    x[7] == '0' &&
    x[8] == '0' &&
    x[9] == '0' &&
    x[10] != '\0' &&
    x[11] != '\0' &&
    x[12] != '\0' &&
    x[13] != '\0' &&
    x[14] != '\0' &&
    x[15] != '\0' &&
    x[16] != '\0' &&
    x[17] != '\0' &&
    x[18] == '\0';
  return o;
}

bool is_normal_AccountID(const char * x) {
  bool o = 
    x[0] == '0' &&
    x[1] == '0' &&
    x[2] == '1' &&
    x[3] == '2' &&
    x[4] == 'P' &&
    x[5] == '0' &&
    x[6] == '0' &&
    x[7] == '0' &&
    x[8] == '0' &&
    x[9] == '0' &&
    x[10] != '\0' &&
    x[11] != '\0' &&
    x[12] != '\0' &&
    x[13] != '\0' &&
    x[14] != '\0' &&
    x[15] != '\0' &&
    x[16] != '\0' &&
    x[17] != '\0' &&
    x[18] == '\0';
  return o;
}

SEXP CCountRecordID(SEXP x) {
  R_xlen_t N = xlength(x);
  if (TYPEOF(x) != STRSXP) {
    return R_NilValue;
  }
  int counters[19][256] = {0};
  
  for (R_xlen_t i = 0; i < N; ++i) {
    const char * xi = CHAR(STRING_ELT(x, i));
    bool normal = is_normal_RecordID(xi);
    if (normal) {
      for (int c = 0; c < 19; ++c) {
        if (xi[c] == '\0') {
          break;
        }
        unsigned int xci = (unsigned int)(xi[c]);
        counters[c][xci] += 1;
      }
    } else {
      int strleni = strlen(xi);
      int p = 18 - strleni;
      for (int c = p; c < 18; ++c) {
        unsigned int xci = (unsigned int)(xi[c - p]);
        counters[c][xci] += 1;
      }
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

SEXP Cclassify_chars(SEXP x, SEXP MaxNchar) {
  R_xlen_t N = xlength(x);
  const int mn = asInteger(MaxNchar);
  SEXP ans = PROTECT(allocVector(INTSXP, mn * 256));
  int * restrict ansp = INTEGER(ans);
  
  // 0-9    2
  // A-Z    3
  // a-z    5
  // Anything else 7
  
  bool digit_classes[mn][256];
  for (int j = 0; j < mn; ++j) {
    for (int c = 0; c < 256; ++c) {
      digit_classes[j][c] = false;
      digit_classes[j][c] = false;
      digit_classes[j][c] = false;
    }
  }
  
  for (R_xlen_t i = 0; i < N; ++i) {
    const char * xi = CHAR(STRING_ELT(x, i));
    int strleni = strlen(xi);
    if (mn == strleni) {
      for (int c = mn - 1; c >= 0; --c) {
        char xic = xi[c];
        unsigned int k = (unsigned int)xic;
        digit_classes[c][k] = true;
      }
      
    } else {
      int nc = 0;
      for (int c = strleni - 1; (c >= 0) && (nc < mn); --c, ++nc) {
        char xic = xi[c];
        unsigned int k = (unsigned int)xic;
        digit_classes[c][k] = true;
      }
    }
  }
  int ii = 0;
  for (int j = 0; j < mn; ++j) {
    for (int k = 0; k < 256; ++k, ++ii) {
      ansp[ii] = digit_classes[j][k];
    }
    
  }
  
  UNPROTECT(1);
  return ans;
}

unsigned int alphnum2uint(char x) {
  if (x < '0' || x > 'z') {
    return 0U;
  }
  if (x <= '9') {
    return x - '0';
  }
  if (x <= 'Z') {
    return 10U + (x - 'A');
  }
  return 10U + 26U + (x - 'a');
}


SEXP Ctabula_RecordID(SEXP x) {
  if (TYPEOF(x) != STRSXP) {
    return R_NilValue;
  }
  R_xlen_t N = xlength(x);
  char tab[19][256];
  for (int j = 0; j < 19; ++j) {
    for (int k = 0; k < 256; ++k) {
      tab[j][k] = 0;
    }
  }
  
  for (R_xlen_t i = 0; i < N; ++i) {
    const char * xi = CHAR(STRING_ELT(x, i));
    int strleni = strlen(xi);
    if (strleni >= 19) {
      strleni = 18;
    }
    for (int c = 0; c < strleni; ++c) {
      char xic = xi[c];
      unsigned int xicj = alphnum2uint(xic);
      tab[c][xicj] = 1;
    }
  }
  SEXP ans = PROTECT(allocVector(INTSXP, 19 * 62));
  int * restrict ansp = INTEGER(ans);
  for (int kk = 0; kk < (19 * 62); ++kk) {
    ansp[kk] = 0;
  }
  int kk = 0;
  for (int i = 0; i < 19; ++i) {
    for (int j = 0; j < 62; ++j, ++kk) {
      if (tab[i][j]) {
        ansp[kk] = 1;
      }
    }
  }
  UNPROTECT(1);
  return ans;
}


SEXP CencodeRecordID(SEXP x) {
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
    //  inds  12,13,14,15, 18  -1 are the only characters that aren't constant
    
    const char * xi = CHAR(STRING_ELT(x, i));
    bool normal = is_normal_RecordID(xi);
    
    if (normal) {
      unsigned int a = 0U, pw = 1U;
      a += pw * alphnum2uint(xi[17]);
      pw *= 62U;
      a += pw * alphnum2uint(xi[14]);
      pw *= 62U;
      a += pw * alphnum2uint(xi[13]);
      pw *= 62U;
      a += pw * alphnum2uint(xi[12]);
      pw *= 62U;
      a += pw * alphnum2uint(xi[11]);
      pw *= 62U;
      a += pw * (alphnum2uint(xi[10]) - 13U);
      ansp[i] = a;
    } else {
      int a = (-INT_MAX) + 1;
      
      unsigned int strleni = strlen(xi);
      strleni = (strleni > 8U) ? 8U : strleni;
      int ten = tens[strleni];
      for (unsigned int c = 0; c < strleni; ++c) {
        char xic = xi[c];
        unsigned int d = xic - '0';
        if (d > 9U) {
          break;
        }
        a += ten * d;
        ten /= 10;
      }
      ansp[i] = a;
    }
  }
  UNPROTECT(1);
  return ans;
}



SEXP CdecodeRecordID(SEXP x) {
  R_xlen_t N = xlength(x);
  if (TYPEOF(x) != INTSXP) {
    error("TYPEOF(x) != INTSXP");
  }
  char string[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
  unsigned int pows[5] = {62U, 62U * 62U, 62U * 62U * 62U, 62U * 62U * 62U * 62U, 
                          62U * 62U * 62U * 62U * 62U};
  
  int * restrict xp = INTEGER(x);
  SEXP ans = PROTECT(allocVector(STRSXP, N));
  for (R_xlen_t i = 0; i < N; ++i) {
    int xpi = xp[i];
    if (xpi < 0) {
      unsigned int nxpi = xpi + INT_MAX - 1;
      int n_digits = n_digits0(nxpi);
      char o[n_digits + 1];
      int ten = 1;
      for (int d = n_digits - 1; d >= 0; --d) {
        o[d] = string[(nxpi / ten) % 10];
        ten *= 10;
      }
      o[n_digits - 1] = '\0';
      const char * oi = o;
      SET_STRING_ELT(ans, i, mkCharCE(oi, CE_UTF8));
      continue;
    }
    
    unsigned int ei = xp[i]; // encoded
    char xi[19];
    xi[0] = '5';
    xi[1] = '0';
    xi[2] = '0';
    xi[3] = '2';
    xi[4] = 'P';
    xi[5] = '0';
    xi[6] = '0';
    xi[7] = '0';
    xi[8] = '0';
    xi[9] = '0';
    
    unsigned int xp10 = ((ei / pows[4] + 13U) % 62U);
    xi[10] = string[xp10];
    
    unsigned int xp11 = (ei / pows[3]) % 62U;
    xi[11] = string[xp11];
    
    unsigned int xp12 = (ei / pows[2]) % 62U;
    xi[12] = string[xp12];
    
    unsigned int xp13 = (ei / pows[1]) % 62U;
    xi[13] = string[xp13];
    
    unsigned int xp14 = (ei / pows[0]) % 62U;
    xi[14] = string[xp14];
    
    xi[15] = 'Q';
    xi[16] = 'A';
    
    unsigned int xp17 = ei % 62U;
    xi[17] = string[xp17];
    
    xi[18] = '\0';
    const char * ansi = xi;
    SET_STRING_ELT(ans, i, mkCharCE(ansi, CE_UTF8));
  }
  UNPROTECT(1);
  return ans;
}

// Encoding mechanism:
//  0 = strlen required
// 

SEXP Cdetermine_const_width_alnum_encoding(SEXP x, SEXP MaxNchar) {
  if (TYPEOF(x) != STRSXP || TYPEOF(MaxNchar) != INTSXP) {
    error("Internal error(Cdetermine_const_width_alnum_encoding): wrong types.");
  }
  R_xlen_t N = xlength(x);
  if (N == 0) {
    return R_NilValue;
  }
  
  
  int max_nchar_tmp = 1;
  R_xlen_t first_non_na = 0;
  if (TYPEOF(MaxNchar) != INTSXP || INTEGER(MaxNchar)[0] == NA_INTEGER) {
    bool auto_maxnchar = TYPEOF(MaxNchar) == NILSXP;
    auto_maxnchar |= TYPEOF(MaxNchar) == INTSXP && INTEGER(MaxNchar)[0] == NA_INTEGER;
    if (auto_maxnchar) {
      // User has requested max_nchar to be based of first non-NA string
      max_nchar_tmp = length(STRING_ELT(x, first_non_na));
      while (first_non_na < N &&
             STRING_ELT(x, first_non_na) == NA_STRING) {
        ++first_non_na;
      }
      if (first_non_na == N) {
        error("`n = NULL`, but x is full of NA.");
      }
    } else if (TYPEOF(MaxNchar) == REALSXP) {
      max_nchar_tmp = (unsigned int)asReal(MaxNchar);
    } else {
      error("MaxNChar wrong type.");
    }
  } else {
    max_nchar_tmp = asInteger(MaxNchar);
  }
  const int max_nchar = max_nchar_tmp;
  if (max_nchar < 0) {
    error("max_nchar is negative (possible NA).");
  }
  unsigned int any_nchar_ge = 0;
  unsigned int any_nchar_le = 0;
  // tbl[62*j + k] is 1 if string[k] is present at position j
  unsigned char * tbl = calloc(max_nchar * 62, sizeof(char));
  if (tbl == NULL) {
    error("(Cdetermine_const_width_alnum_encoding): Unable to allocate tbl.");
  }
  for (R_xlen_t i = 0; i < N; ++i) {
    if (STRING_ELT(x, i) == NA_STRING) {
      continue;
    }
    
    const char * xi = CHAR(STRING_ELT(x, i));
    unsigned int strleni = LENGTH(STRING_ELT(x, i));
    unsigned int base_tbl_j = 0;
    if (strleni == max_nchar) {
      for (unsigned int c = 0; c < max_nchar; ++c) {
        unsigned int tbl_i_a = alphnum2uint(xi[c]);
        tbl[base_tbl_j + tbl_i_a] = 1;
        base_tbl_j += 62U;
      }
    } else if (strleni < max_nchar) {
      any_nchar_le = i;
    } else {
      any_nchar_ge = i;
    }
  }
  if (any_nchar_le) {
    warning("Element %d had strings narrower than max_nchar.", any_nchar_le);
  }
  if (any_nchar_ge) {
    warning("Element %d had strings wider than max_nchar.", any_nchar_ge);
  }
  
  SEXP ans = PROTECT(allocVector(STRSXP, max_nchar));
  int c = 0;
  char string[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
  
  for (int j = 0; j < max_nchar; ++j) {
    unsigned int the_len = 1; // 1 for null terminator
    // the_len is the number of unique characters at position j throughout x
    for (unsigned int cc = 0; cc < 62; ++cc) {
      the_len += tbl[62 * j + cc];
    }
    char ansi[the_len];
    
    int k = 0; // position of ansi
    for (int cc = 0; cc < 62; ++cc, ++c) {
      unsigned char tbl_cc = tbl[62 * j + cc];
      if (tbl_cc) {
        ansi[k] = string[cc];
        ++k;
      }
    }
    ansi[the_len - 1] = '\0';
    const char * ansic = (const char *) ansi;
    SET_STRING_ELT(ans, j, mkCharCE(ansic, CE_UTF8));
  }
  
  free(tbl);
  UNPROTECT(1);
  return ans;
}

SEXP Cvalidate_encoding(SEXP x, SEXP ee) {
  if (TYPEOF(x) != STRSXP || TYPEOF(ee) != STRSXP || xlength(ee) >= INT_MAX) {
    error("Internal error(Cvalidate_encoding): wrong input types.");
  }
  R_xlen_t N = xlength(x);
  const int max_nchar = xlength(ee);
  // check:
  // each element of x has max_nchar elements;
  // each element has only the characters described at each position
  // (it's okay if not all characters are there)
  // Return 1-based index if invalid, 0 otherwise
  
  // Reconstruct tbl from elements
  unsigned char * tbl = calloc(max_nchar * 62, sizeof(char));
  if (tbl == NULL) {
    error("(Cvalidate_encoding): Unable to allocate tbl.");
  }
  
  // memoize
  // alphnum2uint
  unsigned int malphnum2uint[256] = {0};
  char string[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
  for (int i = 0; i < 62; ++i) {
    char si = string[i];
    unsigned int ui = alphnum2uint(si);
    unsigned char sui = (unsigned char)si;
    malphnum2uint[sui] = ui;
  }
  
  for (int j = 0; j < max_nchar; ++j) {
    // populate tbl
    // It is not faster if some positions are constant or unrestricted
    // to special these cases [malphnum2uint is too heroic]
    
    const char * ee_j = CHAR(STRING_ELT(ee, j));
    unsigned int strlenj = strlen(ee_j);
    
    for (int k = 0; k < strlenj; ++k) {
      // ee_j matches
      unsigned int ee_jk = ee_j[k];
      unsigned int dig = malphnum2uint[ee_jk];
      tbl[62 * j + dig] = 1;
    }
  }
  R_xlen_t o = 0;
  for (R_xlen_t i = 0; i < N; ++i) {
    if (STRING_ELT(x, i) == NA_STRING) {
      continue;
    }
    const char * xi = CHAR(STRING_ELT(x, i));
    if (strlen(xi) != max_nchar) {
      o = i + 1;
      break;
    }
    
    for (unsigned int j = 0; j < max_nchar; ++j) {
      // any non alphanum character is valid
      unsigned char xij = xi[j];
      unsigned int k = malphnum2uint[xij];
      unsigned int tk = 62U * j + k;
      if (!tbl[tk]) {
        // if table hasn't been populated, then this character should not 
        // be present.
        o = i + 1;
      }
    }
    
    if (o) {
      break;
    }
  }
  free(tbl);
  return o < INT_MAX ? ScalarInteger(o) : ScalarReal(o);
}

SEXP Calphnum_enc(SEXP x, SEXP ee) {
  if (TYPEOF(x) != STRSXP || TYPEOF(ee) != STRSXP || xlength(ee) >= INT_MAX) {
    error("Internal error(Calphnum_enc): wrong input types.");
  }
  R_xlen_t N = xlength(x);
  const int max_nchar = xlength(ee);
  
  // constant columns can be ignored
  bool char_non_const[max_nchar];
  int non_const = 0;
  for (int j = 0; j < max_nchar; ++j) {
    unsigned int j_len = length(STRING_ELT(ee, j));
    bool j_const = j_len != 1;
    char_non_const[j] = j_const;
    non_const += j_const;
  }
  unsigned int * J = malloc(sizeof(int) * non_const);
  if (J == NULL) {
    error("(Calphnum_enc)Unable to allocate J.");
  }
  for (int j = 0, k = 0; j < max_nchar; ++j) {
    J[k] = j;
    k += char_non_const[j];
  }
  
  // V[256 * j + i] = increment
  unsigned int * V = calloc(256 * non_const, sizeof(int));
  if (V == NULL) {
    error("(Calphnum_enc)Unable to allocate V.");
  }
  for (unsigned int k = 0, b = 1; k < non_const; ++k) {
    int j = J[k];
    const unsigned int ejn = length(STRING_ELT(ee, j));
    const char * ej = CHAR(STRING_ELT(ee, j));
    // loop through the string of ee[j] and assign V that value
    for (unsigned int c = 0; c < ejn; ++c) {
      unsigned char ejc = ej[c];
      unsigned int ejci = (unsigned int)ejc;
      V[256 * k + ejci] = c * b;
    }
    b *= ejn;
  }
  
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  for (R_xlen_t i = 0; i < N; ++i) {
    if (STRING_ELT(x, i) == NA_STRING) {
      ansp[i] = NA_INTEGER;
      continue;
    }
    const char * xi = CHAR(STRING_ELT(x, i));
    unsigned int oi = 0;
    for (int k = 0; k < non_const; ++k) {
      unsigned int j = J[k]; 
      unsigned char xij = xi[j];
      unsigned int xiji = (unsigned int)xij;
      oi += V[256 * k + xiji];
    }
    ansp[i] = (int)oi;
  }
  free(J);
  free(V);
  UNPROTECT(1);
  return ans;
}

SEXP Calphnum_dec(SEXP x, SEXP ee) {
  if (TYPEOF(x) != INTSXP || TYPEOF(ee) != STRSXP || xlength(x) >= INT_MAX) {
    error("Internal error(Calphnum_dec): bad types.");
  }
  int max_nchar = xlength(ee);
  R_xlen_t N = xlength(x);
  
  bool char_non_const[max_nchar];
  unsigned int lens[max_nchar];
  int non_const = 0;
  for (int j = 0; j < max_nchar; ++j) {
    unsigned int j_len = length(STRING_ELT(ee, j));
    lens[j] = j_len;
    bool j_const = j_len != 1;
    char_non_const[j] = j_const;
    non_const += j_const;
  }
  
  if (non_const == 0) {
    error("Cipher implies no non-constant columns.");
  }
  SEXP ans = PROTECT(allocVector(STRSXP, N));
  
  unsigned int * J = malloc(sizeof(int) * non_const);
  if (J == NULL) {
    error("(Calphnum_dec)Unable to allocate J.");
  }
  for (int j = 0, k = 0; j < max_nchar; ++j) {
    J[k] = j;
    k += char_non_const[j];
  }
  const int * xp = INTEGER(x);
  int max_nchar1 = max_nchar + 1;
  
  char default_c[max_nchar1];
  for (int j = 0; j < max_nchar; ++j) {
    default_c[j] = CHAR(STRING_ELT(ee, j))[0];
  }
  default_c[max_nchar] = '\0';
  
  unsigned char C[max_nchar][62];
  for (int j = 0; j < max_nchar; ++j) {
    SEXP eej = STRING_ELT(ee, j);
    int len = length(eej);
    for (int i = 0; i < 62; ++i) {
      if (i < len) {
        C[j][i] = CHAR(STRING_ELT(ee, j))[i];
      } else {
        C[j][i] = '0';
      }
    }
  }
  for (R_xlen_t i = 0; i < N; ++i) {
    unsigned int xi = (unsigned int)xp[i];
    char oi[max_nchar1];
    memcpy(oi, default_c, sizeof(oi));
    for (unsigned int k = 0, b = 1; k < non_const; ++k) {
      int j = J[k];
      if (b > 0u) {
        unsigned int lenj = lens[j];
        unsigned int c = (xi / b) % lenj;
        unsigned char cc = C[j][c];// CHAR(STRING_ELT(ee, j))[c];
        oi[j] = cc;
        b *= lenj;
      }
    }
    oi[max_nchar] = '\0';
    const char * ansi = (const char *)oi;
    SET_STRING_ELT(ans, i, mkCharCE(ansi, CE_UTF8));
  }
  free(J);
  UNPROTECT(1);
  return ans;
}




