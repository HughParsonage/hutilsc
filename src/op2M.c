#include "hutilsc.h"

// const int op = !(eq || gt || lt) ? 0 : (eq ? (gt ? 2 : (lt ? 3 : 1)) : (gt ? 4 : 5));
// != == >= <=  >  <
//  0  1  2  3  4  5


int do_op2M(SEXP op) {
  if (TYPEOF(op) != STRSXP) {
    error("Internal error: op not a character.");
  }
  if (xlength(op) != 1) {
    error("Internal error: op not length 1.");
  }
  const char * opc = CHAR(STRING_ELT(op, 0));
  switch(opc[0]) {
  case '!':
    return OP_NE;
    break;
  case '=':
    return OP_EQ;
    break;
  case '>':
    return (opc[1] == '\0') ? OP_GT : OP_GE;
    break;
  case '<':
    return (opc[1] == '\0') ? OP_LT : OP_LE;
    break;
  case '%':
    switch (opc[1]) {
    case 'i':
      return OP_IN;
      break;
    case 'b':
      return OP_BW;
      break;
    case '(':
      return OP_BO;
      break;
    case ']':
      return OP_BC;
      break;
      
    }
  }
  return 0;
}
