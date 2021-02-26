#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP count_sort_logi(SEXP);
extern SEXP do_and_lgl_int(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP do_and2s(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP do_classify_chars(SEXP, SEXP);
extern SEXP do_clique1(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP do_common_contacts(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP do_counting_sort(SEXP, SEXP);
extern SEXP do_CountRecordID(SEXP);
extern SEXP do_Decode3202(SEXP);
extern SEXP do_decodeRecordID(SEXP);
extern SEXP do_Encode3202(SEXP);
extern SEXP do_encodeRecordID(SEXP);
extern SEXP do_ensure_leq(SEXP, SEXP);
extern SEXP do_fuse1(SEXP, SEXP, SEXP);
extern SEXP do_haversine_distance(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP do_is_constant(SEXP, SEXP);
extern SEXP do_is_sorted(SEXP, SEXP);
extern SEXP do_is_valid_path(SEXP, SEXP, SEXP);
extern SEXP do_minmax(SEXP, SEXP, SEXP);
extern SEXP do_names2int(SEXP, SEXP);
extern SEXP do_packSum(SEXP);
extern SEXP do_pad0(SEXP, SEXP);
extern SEXP do_pmax0(SEXP);
extern SEXP do_tabula_RecordID(SEXP);
extern SEXP do_test_radix_find(SEXP, SEXP, SEXP);
extern SEXP do_test_radix_find_range(SEXP, SEXP);
extern SEXP do_unique_sorted(SEXP);
extern SEXP do_validate_colors(SEXP, SEXP, SEXP);
extern SEXP do_Validate3202(SEXP);
extern SEXP do_which_isnt_int(SEXP);
extern SEXP do_which_isnt_sorted(SEXP);
extern SEXP do_whichminmax(SEXP);
extern SEXP is_binary_call(SEXP);
extern SEXP len3_paths(SEXP, SEXP, SEXP, SEXP);
extern SEXP len4_paths(SEXP, SEXP, SEXP);
extern SEXP lookup4_char(SEXP);
extern SEXP n_sin(SEXP, SEXP, SEXP);
extern SEXP one_edge_dist(SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"count_sort_logi",          (DL_FUNC) &count_sort_logi,           1},
    {"do_and_lgl_int",           (DL_FUNC) &do_and_lgl_int,            5},
    {"do_and2s",                 (DL_FUNC) &do_and2s,                 10},
    {"do_classify_chars",        (DL_FUNC) &do_classify_chars,         2},
    {"do_clique1",               (DL_FUNC) &do_clique1,                5},
    {"do_common_contacts",       (DL_FUNC) &do_common_contacts,        6},
    {"do_counting_sort",         (DL_FUNC) &do_counting_sort,          2},
    {"do_CountRecordID",         (DL_FUNC) &do_CountRecordID,          1},
    {"do_Decode3202",            (DL_FUNC) &do_Decode3202,             1},
    {"do_decodeRecordID",        (DL_FUNC) &do_decodeRecordID,         1},
    {"do_Encode3202",            (DL_FUNC) &do_Encode3202,             1},
    {"do_encodeRecordID",        (DL_FUNC) &do_encodeRecordID,         1},
    {"do_ensure_leq",            (DL_FUNC) &do_ensure_leq,             2},
    {"do_fuse1",                 (DL_FUNC) &do_fuse1,                  3},
    {"do_haversine_distance",    (DL_FUNC) &do_haversine_distance,     5},
    {"do_is_constant",           (DL_FUNC) &do_is_constant,            2},
    {"do_is_sorted",             (DL_FUNC) &do_is_sorted,              2},
    {"do_is_valid_path",         (DL_FUNC) &do_is_valid_path,          3},
    {"do_minmax",                (DL_FUNC) &do_minmax,                 3},
    {"do_names2int",             (DL_FUNC) &do_names2int,              2},
    {"do_packSum",               (DL_FUNC) &do_packSum,                1},
    {"do_pad0",                  (DL_FUNC) &do_pad0,                   2},
    {"do_pmax0",                 (DL_FUNC) &do_pmax0,                  1},
    {"do_tabula_RecordID",       (DL_FUNC) &do_tabula_RecordID,        1},
    {"do_test_radix_find",       (DL_FUNC) &do_test_radix_find,        3},
    {"do_test_radix_find_range", (DL_FUNC) &do_test_radix_find_range,  2},
    {"do_unique_sorted",         (DL_FUNC) &do_unique_sorted,          1},
    {"do_validate_colors",       (DL_FUNC) &do_validate_colors,        3},
    {"do_Validate3202",          (DL_FUNC) &do_Validate3202,           1},
    {"do_which_isnt_int",        (DL_FUNC) &do_which_isnt_int,         1},
    {"do_which_isnt_sorted",     (DL_FUNC) &do_which_isnt_sorted,      1},
    {"do_whichminmax",           (DL_FUNC) &do_whichminmax,            1},
    {"is_binary_call",           (DL_FUNC) &is_binary_call,            1},
    {"len3_paths",               (DL_FUNC) &len3_paths,                4},
    {"len4_paths",               (DL_FUNC) &len4_paths,                3},
    {"lookup4_char",             (DL_FUNC) &lookup4_char,              1},
    {"n_sin",                    (DL_FUNC) &n_sin,                     3},
    {"one_edge_dist",            (DL_FUNC) &one_edge_dist,             4},
    {NULL, NULL, 0}
};

void R_init_hutilsc(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
