#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP do_and_lgl_int(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP do_and2s(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP do_Decode3202(SEXP);
extern SEXP do_Encode3202(SEXP);
extern SEXP do_haversine_distance(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP do_minmax(SEXP, SEXP, SEXP);
extern SEXP do_names2int(SEXP, SEXP);
extern SEXP do_pmax0(SEXP);
extern SEXP do_test_radix_find(SEXP, SEXP, SEXP);
extern SEXP do_test_radix_find_range(SEXP, SEXP);
extern SEXP is_binary_call(SEXP);
extern SEXP lookup2_char(SEXP);
extern SEXP lookup4_char(SEXP);
extern SEXP one_edge_dist(SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"do_and_lgl_int",           (DL_FUNC) &do_and_lgl_int,           5},
    {"do_and2s",                 (DL_FUNC) &do_and2s,                 9},
    {"do_Decode3202",            (DL_FUNC) &do_Decode3202,            1},
    {"do_Encode3202",            (DL_FUNC) &do_Encode3202,            1},
    {"do_haversine_distance",    (DL_FUNC) &do_haversine_distance,    5},
    {"do_minmax",                (DL_FUNC) &do_minmax,                3},
    {"do_names2int",             (DL_FUNC) &do_names2int,             2},
    {"do_pmax0",                 (DL_FUNC) &do_pmax0,                 1},
    {"do_test_radix_find",       (DL_FUNC) &do_test_radix_find,       3},
    {"do_test_radix_find_range", (DL_FUNC) &do_test_radix_find_range, 2},
    {"is_binary_call",           (DL_FUNC) &is_binary_call,           1},
    {"lookup2_char",             (DL_FUNC) &lookup2_char,             1},
    {"lookup4_char",             (DL_FUNC) &lookup4_char,             1},
    {"one_edge_dist",            (DL_FUNC) &one_edge_dist,            4},
    {NULL, NULL, 0}
};

void R_init_hutilsc(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
