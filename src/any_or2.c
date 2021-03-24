#include "hutilsc.h"

// input types

#define INPUT2_IIII 1
#define INPUT2_IIIR 2
#define INPUT2_IIIi 3
#define INPUT2_IIIr 4
#define INPUT2_IRII 5
#define INPUT2_IRIR 6
#define INPUT2_IRIi 7
#define INPUT2_IRIr 8
#define INPUT2_IiII 9
#define INPUT2_IiIR 10
#define INPUT2_IiIi 11
#define INPUT2_IiIr 12
#define INPUT2_IrII 13
#define INPUT2_IrIR 14
#define INPUT2_IrIi 15
#define INPUT2_IrIr 16
#define INPUT2_IIRI 17
#define INPUT2_IIRR 18
#define INPUT2_IIRi 19
#define INPUT2_IIRr 20
#define INPUT2_IRRI 21
#define INPUT2_IRRR 22
#define INPUT2_IRRi 23
#define INPUT2_IRRr 24
#define INPUT2_IiRI 25
#define INPUT2_IiRR 26
#define INPUT2_IiRi 27
#define INPUT2_IiRr 28
#define INPUT2_IrRI 29
#define INPUT2_IrRR 30
#define INPUT2_IrRi 31
#define INPUT2_IrRr 32
#define INPUT2_IIiI 33
#define INPUT2_IIiR 34
#define INPUT2_IIii 35
#define INPUT2_IIir 36
#define INPUT2_IRiI 37
#define INPUT2_IRiR 38
#define INPUT2_IRii 39
#define INPUT2_IRir 40
#define INPUT2_IiiI 41
#define INPUT2_IiiR 42
#define INPUT2_Iiii 43
#define INPUT2_Iiir 44
#define INPUT2_IriI 45
#define INPUT2_IriR 46
#define INPUT2_Irii 47
#define INPUT2_Irir 48
#define INPUT2_IIrI 49
#define INPUT2_IIrR 50
#define INPUT2_IIri 51
#define INPUT2_IIrr 52
#define INPUT2_IRrI 53
#define INPUT2_IRrR 54
#define INPUT2_IRri 55
#define INPUT2_IRrr 56
#define INPUT2_IirI 57
#define INPUT2_IirR 58
#define INPUT2_Iiri 59
#define INPUT2_Iirr 60
#define INPUT2_IrrI 61
#define INPUT2_IrrR 62
#define INPUT2_Irri 63
#define INPUT2_Irrr 64
#define INPUT2_RIII 65
#define INPUT2_RIIR 66
#define INPUT2_RIIi 67
#define INPUT2_RIIr 68
#define INPUT2_RRII 69
#define INPUT2_RRIR 70
#define INPUT2_RRIi 71
#define INPUT2_RRIr 72
#define INPUT2_RiII 73
#define INPUT2_RiIR 74
#define INPUT2_RiIi 75
#define INPUT2_RiIr 76
#define INPUT2_RrII 77
#define INPUT2_RrIR 78
#define INPUT2_RrIi 79
#define INPUT2_RrIr 80
#define INPUT2_RIRI 81
#define INPUT2_RIRR 82
#define INPUT2_RIRi 83
#define INPUT2_RIRr 84
#define INPUT2_RRRI 85
#define INPUT2_RRRR 86
#define INPUT2_RRRi 87
#define INPUT2_RRRr 88
#define INPUT2_RiRI 89
#define INPUT2_RiRR 90
#define INPUT2_RiRi 91
#define INPUT2_RiRr 92
#define INPUT2_RrRI 93
#define INPUT2_RrRR 94
#define INPUT2_RrRi 95
#define INPUT2_RrRr 96
#define INPUT2_RIiI 97
#define INPUT2_RIiR 98
#define INPUT2_RIii 99
#define INPUT2_RIir 100
#define INPUT2_RRiI 101
#define INPUT2_RRiR 102
#define INPUT2_RRii 103
#define INPUT2_RRir 104
#define INPUT2_RiiI 105
#define INPUT2_RiiR 106
#define INPUT2_Riii 107
#define INPUT2_Riir 108
#define INPUT2_RriI 109
#define INPUT2_RriR 110
#define INPUT2_Rrii 111
#define INPUT2_Rrir 112
#define INPUT2_RIrI 113
#define INPUT2_RIrR 114
#define INPUT2_RIri 115
#define INPUT2_RIrr 116
#define INPUT2_RRrI 117
#define INPUT2_RRrR 118
#define INPUT2_RRri 119
#define INPUT2_RRrr 120
#define INPUT2_RirI 121
#define INPUT2_RirR 122
#define INPUT2_Riri 123
#define INPUT2_Rirr 124
#define INPUT2_RrrI 125
#define INPUT2_RrrR 126
#define INPUT2_Rrri 127
#define INPUT2_Rrrr 128
#define INPUT2_iIII 129
#define INPUT2_iIIR 130
#define INPUT2_iIIi 131
#define INPUT2_iIIr 132
#define INPUT2_iRII 133
#define INPUT2_iRIR 134
#define INPUT2_iRIi 135
#define INPUT2_iRIr 136
#define INPUT2_iiII 137
#define INPUT2_iiIR 138
#define INPUT2_iiIi 139
#define INPUT2_iiIr 140
#define INPUT2_irII 141
#define INPUT2_irIR 142
#define INPUT2_irIi 143
#define INPUT2_irIr 144
#define INPUT2_iIRI 145
#define INPUT2_iIRR 146
#define INPUT2_iIRi 147
#define INPUT2_iIRr 148
#define INPUT2_iRRI 149
#define INPUT2_iRRR 150
#define INPUT2_iRRi 151
#define INPUT2_iRRr 152
#define INPUT2_iiRI 153
#define INPUT2_iiRR 154
#define INPUT2_iiRi 155
#define INPUT2_iiRr 156
#define INPUT2_irRI 157
#define INPUT2_irRR 158
#define INPUT2_irRi 159
#define INPUT2_irRr 160
#define INPUT2_iIiI 161
#define INPUT2_iIiR 162
#define INPUT2_iIii 163
#define INPUT2_iIir 164
#define INPUT2_iRiI 165
#define INPUT2_iRiR 166
#define INPUT2_iRii 167
#define INPUT2_iRir 168
#define INPUT2_iiiI 169
#define INPUT2_iiiR 170
#define INPUT2_iiii 171
#define INPUT2_iiir 172
#define INPUT2_iriI 173
#define INPUT2_iriR 174
#define INPUT2_irii 175
#define INPUT2_irir 176
#define INPUT2_iIrI 177
#define INPUT2_iIrR 178
#define INPUT2_iIri 179
#define INPUT2_iIrr 180
#define INPUT2_iRrI 181
#define INPUT2_iRrR 182
#define INPUT2_iRri 183
#define INPUT2_iRrr 184
#define INPUT2_iirI 185
#define INPUT2_iirR 186
#define INPUT2_iiri 187
#define INPUT2_iirr 188
#define INPUT2_irrI 189
#define INPUT2_irrR 190
#define INPUT2_irri 191
#define INPUT2_irrr 192
#define INPUT2_rIII 193
#define INPUT2_rIIR 194
#define INPUT2_rIIi 195
#define INPUT2_rIIr 196
#define INPUT2_rRII 197
#define INPUT2_rRIR 198
#define INPUT2_rRIi 199
#define INPUT2_rRIr 200
#define INPUT2_riII 201
#define INPUT2_riIR 202
#define INPUT2_riIi 203
#define INPUT2_riIr 204
#define INPUT2_rrII 205
#define INPUT2_rrIR 206
#define INPUT2_rrIi 207
#define INPUT2_rrIr 208
#define INPUT2_rIRI 209
#define INPUT2_rIRR 210
#define INPUT2_rIRi 211
#define INPUT2_rIRr 212
#define INPUT2_rRRI 213
#define INPUT2_rRRR 214
#define INPUT2_rRRi 215
#define INPUT2_rRRr 216
#define INPUT2_riRI 217
#define INPUT2_riRR 218
#define INPUT2_riRi 219
#define INPUT2_riRr 220
#define INPUT2_rrRI 221
#define INPUT2_rrRR 222
#define INPUT2_rrRi 223
#define INPUT2_rrRr 224
#define INPUT2_rIiI 225
#define INPUT2_rIiR 226
#define INPUT2_rIii 227
#define INPUT2_rIir 228
#define INPUT2_rRiI 229
#define INPUT2_rRiR 230
#define INPUT2_rRii 231
#define INPUT2_rRir 232
#define INPUT2_riiI 233
#define INPUT2_riiR 234
#define INPUT2_riii 235
#define INPUT2_riir 236
#define INPUT2_rriI 237
#define INPUT2_rriR 238
#define INPUT2_rrii 239
#define INPUT2_rrir 240
#define INPUT2_rIrI 241
#define INPUT2_rIrR 242
#define INPUT2_rIri 243
#define INPUT2_rIrr 244
#define INPUT2_rRrI 245
#define INPUT2_rRrR 246
#define INPUT2_rRri 247
#define INPUT2_rRrr 248
#define INPUT2_rirI 249
#define INPUT2_rirR 250
#define INPUT2_riri 251
#define INPUT2_rirr 252
#define INPUT2_rrrI 253
#define INPUT2_rrrR 254
#define INPUT2_rrri 255
#define INPUT2_rrrr 256



static int input_types(SEXP X1, SEXP Y1, 
                       SEXP X2, SEXP Y2) {
  int nx1 = xlength(X1) == 1;
  int ny1 = xlength(Y1) == 1;
  int nx2 = xlength(X2) == 1;
  int ny2 = xlength(Y2) == 1;
  
  return 
    128 * nx1 + 
     64 * (TYPEOF(X1) == REALSXP) + 
     32 * nx2 + 
     16 * (TYPEOF(X2) == REALSXP) + 
       8 * ny1 + 
       4 * (TYPEOF(Y1) == REALSXP) + 
       2 * ny2 + 
       1 * (TYPEOF(Y2) == REALSXP) + 
       1; // add one to make zero special
}

SEXP test_input_types(SEXP X1, SEXP Y1, 
                      SEXP X2, SEXP Y2) {
  return ScalarInteger(input_types(X1, Y1, X2, Y2));
}
  

static bool xiopyi(int x, int o, int y) {
  switch(o) {
  case OP_NE:
    return x != y;
  case OP_IN:
  case OP_EQ:
    return x == y;
  case OP_GE:
    return x >= y;
  case OP_LE:
    return x <= y;
  case OP_GT:
    return x > y;
  case OP_LT:
    return x < y;
  }
  return false; // # nocov
}
static bool xiopyd(int x, int o, double y) {
  switch(o) {
  case OP_NE:
    return x != y;
  case OP_IN:
  case OP_EQ:
    return x == y;
  case OP_GE:
    return x >= y;
  case OP_LE:
    return x <= y;
  case OP_GT:
    return x > y;
  case OP_LT:
    return x < y;
  }
  return false; // # nocov
}

static bool xdopyi(double x, int o, int y) {
  switch(o) {
  case OP_NE:
    return x != y;
  case OP_IN:
  case OP_EQ:
    return x == y;
  case OP_GE:
    return x >= y;
  case OP_LE:
    return x <= y;
  case OP_GT:
    return x > y;
  case OP_LT:
    return x < y;
  }
  return false; // # nocov
}

static bool xdopyd(double x, int o, double y) {
  switch(o) {
  case OP_NE:
    return x != y;
  case OP_IN:
  case OP_EQ:
    return x == y;
  case OP_GE:
    return x >= y;
  case OP_LE:
    return x <= y;
  case OP_GT:
    return x > y;
  case OP_LT:
    return x < y;
  }
  return false; // # nocov
}

// ii = x1 and x2 are integers
bool any_or2_ii(SEXP X1, SEXP O1, SEXP Y1,
                SEXP X2, SEXP O2, SEXP Y2, int nthreads) {
  R_xlen_t N = xlength(X1);
  const int o1 = asInteger(O1);
  const int o2 = asInteger(O2);
  const int * x1p = INTEGER(X1);
  const int * x2p = INTEGER(X2);
  
  switch(input_types(X1, Y1, X2, Y2)) {
  case INPUT2_IiIi: {
    const int y1 = asInteger(Y1);
    const int y2 = asInteger(Y2);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xiopyi(x1p[i], o1, y1) || 
          xiopyi(x2p[i], o2, y2)) {
        return true;
      }
    }
    return false;
  }
  case INPUT2_IiII: {
    const int y1 = asInteger(Y1);
    const int * y2p = INTEGER(Y2);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xiopyi(x1p[i], o1, y1) || 
          xiopyi(x2p[i], o2, y2p[i])) {
        return true;
      }
    }
    return false;
  }
  case INPUT2_IIIi: {
    const int * y1p = INTEGER(Y1);
    const int y2 = asInteger(Y2);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xiopyi(x1p[i], o1, y1p[i]) || 
          xiopyi(x2p[i], o2, y2)) {
        return true;
      }
    }
    return false;
  }
  case INPUT2_IIII: {
    const int * y1p = INTEGER(Y1);
    const int * y2p = INTEGER(Y2);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xiopyi(x1p[i], o1, y1p[i]) || 
          xiopyi(x2p[i], o2, y2p[i])) {
        return true;
      }
    }
    return false;
  }
  case INPUT2_IRIR: {
    const double * y1p = REAL(Y1);
    const double * y2p = REAL(Y2);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xiopyd(x1p[i], o1, y1p[i]) || 
          xiopyd(x2p[i], o2, y2p[i])) {
        return true;
      }
    }
    return false;
  }
  case INPUT2_IrIr: {
    const double y1 = asReal(Y1);
    const double y2 = asReal(Y2);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xiopyd(x1p[i], o1, y1) || 
          xiopyd(x2p[i], o2, y2)) {
        return true;
      }
    }
    return false;
  }
  // real rhs
  
  case INPUT2_IiIr: {
    const int y1 = asInteger(Y1);
    const double y2 = asReal(Y2);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xiopyi(x1p[i], o1, y1) || 
          xiopyd(x2p[i], o2, y2)) {
        return true;
      }
    }
    return false;
  }
  case INPUT2_IiIR: {
    const int y1 = asInteger(Y1);
    const double * y2p = REAL(Y2);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xiopyi(x1p[i], o1, y1) || 
          xiopyd(x2p[i], o2, y2p[i])) {
        return true;
      }
    }
    return false;
  }
  case INPUT2_IIIr: {
    const int * y1p = INTEGER(Y1);
    const double y2 = asReal(Y2);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xiopyi(x1p[i], o1, y1p[i]) || 
          xiopyd(x2p[i], o2, y2)) {
        return true;
      }
    }
    return false;
  }
  case INPUT2_IIIR: {
    const int * y1p = INTEGER(Y1);
    const double * y2p = REAL(Y2);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xiopyi(x1p[i], o1, y1p[i]) || 
          xiopyd(x2p[i], o2, y2p[i])) {
        return true;
      }
    }
    return false;
  }
  
  case INPUT2_IRII: {
    const double * y1p = REAL(Y1);
    const int * y2p = INTEGER(Y2);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xiopyd(x1p[i], o1, y1p[i]) ||
          xiopyi(x2p[i], o2, y2p[i])) {
        return true;
      }
    }
    return false;
  }
  case INPUT2_IRIi: {
    const double * y1p = REAL(Y1);
    const int y2 = asInteger(Y2);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xiopyd(x1p[i], o1, y1p[i]) ||
          xiopyi(x2p[i], o2, y2)) {
        return true;
      }
    }
    return false;
  }
  case INPUT2_IrII: {
    const double y1 = asReal(Y1);
    const int * y2p = INTEGER(Y2);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xiopyd(x1p[i], o1, y1) ||
          xiopyd(x2p[i], o2, y2p[i])) {
        return true;
      }
    }
    return false;
  }
  case INPUT2_IrIi: {
    const double y1 = asReal(Y1);
    const int y2 = asInteger(Y2);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xiopyd(x1p[i], o1, y1) ||
          xiopyd(x2p[i], o2, y2)) {
        return true;
      }
    }
    return false;
  }
  case INPUT2_IRIr: {
    const double * y1p = REAL(Y1);
    const double y2 = asReal(Y2);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xiopyd(x1p[i], o1, y1p[i]) ||
          xiopyd(x2p[i], o2, y2)) {
        return true;
      }
    }
    return false;
  }
  case INPUT2_IrIR: {
    const double y1 = asReal(Y1);
    const double * y2p = REAL(Y2);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xiopyd(x1p[i], o1, y1) ||
          xiopyd(x2p[i], o2, y2p[i])) {
        return true;
      }
    }
    return false;
  }
  }
  return false;
}

bool any_or2_id(SEXP X1, SEXP O1, SEXP Y1,
                SEXP X2, SEXP O2, SEXP Y2) {
  R_xlen_t N = xlength(X1);
  const int o1 = asInteger(O1);
  const int o2 = asInteger(O2);
  const int * x1p = INTEGER(X1);
  const double * x2p = REAL(X2);
  
  const int input2 = input_types(X1, Y1, X2, Y2);
  switch(input2) {
  
  case INPUT2_IiRi: {
    const int y1 = asInteger(Y1);
    const int y2 = asInteger(Y2);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xiopyi(x1p[i], o1, y1) ||
          xdopyi(x2p[i], o2, y2)) {
        return true;
      }
    }
    return false;
  }
  case INPUT2_IiRI: {
    const int y1 = asInteger(Y1);
    const int * y2p = INTEGER(Y2);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xiopyi(x1p[i], o1, y1) ||
          xdopyi(x2p[i], o2, y2p[i])) {
        return true;
      }
    }
    return false;
  }
  case INPUT2_IIRi: {
      const int * y1p = INTEGER(Y1);
      const int y2 = asInteger(Y2);
      for (R_xlen_t i = 0; i < N; ++i) {
        if (xiopyi(x1p[i], o1, y1p[i]) ||
            xdopyi(x2p[i], o2, y2)) {
          return true;
        }
      }
      return false;
    }
  case INPUT2_IIRI: {
    const int * y1p = INTEGER(Y1);
    const int * y2p = INTEGER(Y2);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xiopyi(x1p[i], o1, y1p[i]) ||
          xdopyi(x2p[i], o2, y2p[i])) {
        return true;
      }
    }
    return false;
  }
  case INPUT2_IiRr: {
      const int y1 = asInteger(Y1);
      const double y2 = asReal(Y2);
      for (R_xlen_t i = 0; i < N; ++i) {
        if (xiopyi(x1p[i], o1, y1) ||
            xdopyd(x2p[i], o2, y2)) {
          return true;
        }
      }
      return false;
    }
  case INPUT2_IiRR: {
    const int y1 = asInteger(Y1);
    const double * y2p = REAL(Y2);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xiopyi(x1p[i], o1, y1) ||
          xdopyi(x2p[i], o2, y2p[i])) {
        return true;
      }
    }
    return false;
  }
  case INPUT2_IIRr:{
    const int * y1p = INTEGER(Y1);
    const double y2 = asReal(Y2);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xiopyi(x1p[i], o1, y1p[i]) ||
          xdopyd(x2p[i], o2, y2)) {
        return true;
      }
    }
    return false;
  }
  case INPUT2_IIRR: {
    const int * y1p = INTEGER(Y1);
    const double * y2p = REAL(Y2);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xiopyi(x1p[i], o1, y1p[i]) ||
          xdopyd(x2p[i], o2, y2p[i])) {
        return true;
      }
    }
    return false;
  }
  case INPUT2_IRRR: {
    const double * y1p = REAL(Y1);
    const double * y2p = REAL(Y2);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xiopyd(x1p[i], o1, y1p[i]) ||
          xdopyd(x2p[i], o2, y2p[i])) {
        return true;
      }
    }
    return false;
  }
  case INPUT2_IrRr: {
    const double y1 = asReal(Y1);
    const double y2 = asReal(Y2);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xiopyd(x1p[i], o1, y1) ||
          xdopyd(x2p[i], o2, y2)) {
        return true;
      }
    }
    return false;
  }
  case INPUT2_IrRi: {
    const double y1 = asReal(Y1);
    const int y2 = asInteger(Y2);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xiopyd(x1p[i], o1, y1) ||
          xdopyi(x2p[i], o2, y2)) {
        return true;
      }
    }
    return false;
  }
  case INPUT2_IrRR: {
    const double y1 = asReal(Y1);
    const double * y2p = REAL(Y2);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xiopyd(x1p[i], o1, y1) ||
          xdopyd(x2p[i], o2, y2p[i])) {
        return true;
      }
    }
    return false;
  }
  case INPUT2_IRRr: {
    const double * y1p = REAL(Y1);
    const double y2 = asReal(Y2);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xiopyd(x1p[i], o1, y1p[i]) ||
          xdopyd(x2p[i], o2, y2)) {
        return true;
      }
    }
    return false;
  }
  case INPUT2_IRRI: {
    const double * y1p = REAL(Y1);
    const int * y2p = INTEGER(Y2);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xiopyd(x1p[i], o1, y1p[i]) ||
          xdopyi(x2p[i], o2, y2p[i])) {
        return true;
      }
    }
    return false;
  }
  case INPUT2_IRRi: {
    const double * y1p = REAL(Y1);
    const int y2 = asInteger(Y2);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xiopyd(x1p[i], o1, y1p[i]) ||
          xdopyi(x2p[i], o2, y2)) {
        return true;
      }
    }
    return false;
  }
  case INPUT2_IrRI: {
    const double y1 = asReal(Y1);
    const int * y2p = INTEGER(Y2);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xiopyd(x1p[i], o1, y1) ||
          xdopyi(x2p[i], o2, y2p[i])) {
        return true;
      }
    }
    return false;
  }
  }
  
  return false;
}


bool any_xoy(SEXP X, SEXP O, SEXP Y) {
  R_xlen_t N = xlength(X);
  const int o = asInteger(O);
  
  if (TYPEOF(X) == INTSXP && TYPEOF(Y) == INTSXP && xlength(Y) == N) {
    const int * xp = INTEGER(X);
    const int * yp = INTEGER(Y);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xiopyi(xp[i], o, yp[i])) {
        return true;
      }
    }
    return false;
  }
  if (TYPEOF(X) == INTSXP && TYPEOF(Y) == INTSXP && xlength(Y) == 1) {
    const int * xp = INTEGER(X);
    const int y = asInteger(Y);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xiopyi(xp[i], o, y)) {
        return true;
      }
    }
    return false;
  }
  if (TYPEOF(X) == INTSXP && TYPEOF(Y) == REALSXP && xlength(Y) == N) {
    const int * xp = INTEGER(X);
    const double * yp = REAL(Y);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xiopyd(xp[i], o, yp[i])) {
        return true;
      }
    }
    return false;
  }
  if (TYPEOF(X) == INTSXP && TYPEOF(Y) == REALSXP && xlength(Y) == 1) {
    const int * xp = INTEGER(X);
    const double y = asReal(Y);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xiopyd(xp[i], o, y)) {
        return true;
      }
    }
    return false;
  }
  return false;
}


SEXP Cany_or2(SEXP X1, SEXP O1, SEXP Y1,
                SEXP X2, SEXP O2, SEXP Y2,
                SEXP nThread) {
  R_xlen_t N = xlength(X1);
  if (N == 1) {
    return R_NilValue;
  }
  if (TYPEOF(X1) > TYPEOF(X2)) {
    return Cany_or2(X2, O2, Y2, X1, O1, Y1, nThread);
  }
  
  if (xlength(Y1) != N && xlength(Y1) != 1) {
    return R_NilValue;
  }
  if (xlength(X2) != N && xlength(X2) != 1) {
    return R_NilValue;
  }
  if (xlength(Y2) != N && xlength(Y2) != 1) {
    return R_NilValue;
  }
  if (TYPEOF(nThread) != INTSXP) {
    return R_NilValue;
  }
  int nthreads = asInteger(nThread);
  const int o2 = asInteger(O2);
  
  if (xlength(X2) == 1) {
    if (xlength(Y2) != 1) {
      return R_NilValue;
    }
    if (TYPEOF(X2) == INTSXP && TYPEOF(Y2) == INTSXP) {
      const int x2 = asInteger(X2);
      const int y2 = asInteger(Y2);
      if (xiopyi(x2, o2, y2)) {
        return ScalarLogical(1);
      }
      // just need to check x1 o1 y1
      if (any_xoy(X1, O1, Y1)) {
        return ScalarLogical(1);
      }
      return ScalarLogical(0);
    }
    
    return R_NilValue;
  }
  if (TYPEOF(X1) == INTSXP && TYPEOF(X2) == INTSXP) {
    if (!Rf_isNumeric(Y1) || !Rf_isNumeric(Y2)) {
      return R_NilValue;
    }
    bool o = false;
    o = any_or2_ii(X1, O1, Y1, X2, O2, Y2, nthreads);
    return ScalarLogical(o);
  }
  if (TYPEOF(X1) == INTSXP && TYPEOF(X2) == REALSXP) {
    if (!isNumeric(Y1) || !isNumeric(Y2)) {
      return R_NilValue;
    }
    bool o = false;
    o = any_or2_id(X1, O1, Y1, X2, O2, Y2);
    return ScalarLogical(o);
  }
  return R_NilValue;
}

SEXP Ccomplete_cases(SEXP x) {
  R_xlen_t N = xlength(x);
  if (TYPEOF(x) != INTSXP) {
    error("Not integer.");
  }
  SEXP ans = PROTECT(allocVector(LGLSXP, N));
  int *restrict ansp = LOGICAL(ans);
  const int *xp = INTEGER(x);
  for (R_xlen_t i = 0; i < N; ++i) {
    ansp[i] = xp[i] != NA_INTEGER;
  }
  UNPROTECT(1);
  return ans;
}
