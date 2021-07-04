#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP C_LayoutFruchtermanReingold1(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP C_nFirstOrder(SEXP, SEXP, SEXP, SEXP);
extern SEXP C_nPathsBetween_GivenDist(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP Callocate0_int(SEXP, SEXP);
extern SEXP Calphnum_dec(SEXP, SEXP);
extern SEXP Calphnum_enc(SEXP, SEXP);
extern SEXP Cand_lgl_int(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP Cand2s(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP Cany_or2(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP Cclassify_chars(SEXP, SEXP);
extern SEXP Cclique1(SEXP, SEXP, SEXP, SEXP);
extern SEXP Ccollatz(SEXP);
extern SEXP Ccommon_contacts(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP CCountRaws(SEXP, SEXP);
extern SEXP CCountRecordID(SEXP);
extern SEXP CDecode3202(SEXP);
extern SEXP CdecodeRecordID(SEXP);
extern SEXP Cdetermine_const_width_alnum_encoding(SEXP, SEXP);
extern SEXP Cdist_bw_edges(SEXP, SEXP, SEXP);
extern SEXP CDist2(SEXP, SEXP, SEXP);
extern SEXP Cego_net(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP CEncode3202(SEXP);
extern SEXP CencodeRecordID(SEXP);
extern SEXP Censeq(SEXP);
extern SEXP CEnsure_fwc(SEXP);
extern SEXP Censure_leq(SEXP, SEXP);
extern SEXP Cevery_int32(SEXP, SEXP);
extern SEXP Cfast_nchar(SEXP);
extern SEXP Cfibonacci(SEXP, SEXP);
extern SEXP Cfind_ftc(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP CfindAbsent(SEXP, SEXP);
extern SEXP Cfuse3(SEXP, SEXP, SEXP, SEXP);
extern SEXP Chaversine_distance(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP Cis_constant(SEXP, SEXP);
extern SEXP Cis_sorted(SEXP, SEXP);
extern SEXP Cis_valid_path(SEXP, SEXP, SEXP);
extern SEXP CkuniqueN(SEXP);
extern SEXP Cminmax(SEXP, SEXP, SEXP);
extern SEXP Cn_paths_svt0(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP Cnames2int(SEXP, SEXP);
extern SEXP Cnchar(SEXP);
extern SEXP count_sort_logi(SEXP);
extern SEXP CpackSum(SEXP, SEXP);
extern SEXP Cpad0(SEXP, SEXP);
extern SEXP Cpcg_hash(SEXP, SEXP, SEXP, SEXP);
extern SEXP Cpmax0(SEXP);
extern SEXP Crange_nchar(SEXP);
extern SEXP CShuffleRindex(SEXP);
extern SEXP Csimulate_racf(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP Csqrt2(SEXP);
extern SEXP Csum_int(SEXP);
extern SEXP Csum_isna(SEXP, SEXP);
extern SEXP CSumRaw(SEXP, SEXP);
extern SEXP Ctabula_RecordID(SEXP);
extern SEXP Ctest_find_first(SEXP, SEXP, SEXP);
extern SEXP Ctest_qru(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP Ctest_radix_find(SEXP, SEXP, SEXP);
extern SEXP Ctest_radix_find_range(SEXP, SEXP, SEXP);
extern SEXP Cunique_sorted(SEXP);
extern SEXP Cvalidate_clique(SEXP, SEXP, SEXP, SEXP);
extern SEXP Cvalidate_encoding(SEXP, SEXP);
extern SEXP CValidate3202(SEXP);
extern SEXP Cwhich_isnt_int(SEXP);
extern SEXP Cwhich_isnt_sorted(SEXP);
extern SEXP Cwhichminmax(SEXP);
extern SEXP do_between(SEXP, SEXP, SEXP, SEXP);
extern SEXP do_bsearch(SEXP, SEXP);
extern SEXP is_binary_call(SEXP);
extern SEXP len3_paths(SEXP, SEXP, SEXP);
extern SEXP len4_paths(SEXP, SEXP, SEXP, SEXP);
extern SEXP lookup4_char(SEXP);
extern SEXP qgraph_layout_Cpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP showsqrt_fast(SEXP);
extern SEXP test_input_types(SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"C_LayoutFruchtermanReingold1",          (DL_FUNC) &C_LayoutFruchtermanReingold1,           6},
    {"C_nFirstOrder",                         (DL_FUNC) &C_nFirstOrder,                          4},
    {"C_nPathsBetween_GivenDist",             (DL_FUNC) &C_nPathsBetween_GivenDist,              6},
    {"Callocate0_int",                        (DL_FUNC) &Callocate0_int,                         2},
    {"Calphnum_dec",                          (DL_FUNC) &Calphnum_dec,                           2},
    {"Calphnum_enc",                          (DL_FUNC) &Calphnum_enc,                           2},
    {"Cand_lgl_int",                          (DL_FUNC) &Cand_lgl_int,                           5},
    {"Cand2s",                                (DL_FUNC) &Cand2s,                                10},
    {"Cany_or2",                              (DL_FUNC) &Cany_or2,                               7},
    {"Cclassify_chars",                       (DL_FUNC) &Cclassify_chars,                        2},
    {"Cclique1",                              (DL_FUNC) &Cclique1,                               4},
    {"Ccollatz",                              (DL_FUNC) &Ccollatz,                               1},
    {"Ccommon_contacts",                      (DL_FUNC) &Ccommon_contacts,                       6},
    {"CCountRaws",                            (DL_FUNC) &CCountRaws,                             2},
    {"CCountRecordID",                        (DL_FUNC) &CCountRecordID,                         1},
    {"CDecode3202",                           (DL_FUNC) &CDecode3202,                            1},
    {"CdecodeRecordID",                       (DL_FUNC) &CdecodeRecordID,                        1},
    {"Cdetermine_const_width_alnum_encoding", (DL_FUNC) &Cdetermine_const_width_alnum_encoding,  2},
    {"Cdist_bw_edges",                        (DL_FUNC) &Cdist_bw_edges,                         3},
    {"CDist2",                                (DL_FUNC) &CDist2,                                 3},
    {"Cego_net",                              (DL_FUNC) &Cego_net,                               7},
    {"CEncode3202",                           (DL_FUNC) &CEncode3202,                            1},
    {"CencodeRecordID",                       (DL_FUNC) &CencodeRecordID,                        1},
    {"Censeq",                                (DL_FUNC) &Censeq,                                 1},
    {"CEnsure_fwc",                           (DL_FUNC) &CEnsure_fwc,                            1},
    {"Censure_leq",                           (DL_FUNC) &Censure_leq,                            2},
    {"Cevery_int32",                          (DL_FUNC) &Cevery_int32,                           2},
    {"Cfast_nchar",                           (DL_FUNC) &Cfast_nchar,                            1},
    {"Cfibonacci",                            (DL_FUNC) &Cfibonacci,                             2},
    {"Cfind_ftc",                             (DL_FUNC) &Cfind_ftc,                              5},
    {"CfindAbsent",                           (DL_FUNC) &CfindAbsent,                            2},
    {"Cfuse3",                                (DL_FUNC) &Cfuse3,                                 4},
    {"Chaversine_distance",                   (DL_FUNC) &Chaversine_distance,                    5},
    {"Cis_constant",                          (DL_FUNC) &Cis_constant,                           2},
    {"Cis_sorted",                            (DL_FUNC) &Cis_sorted,                             2},
    {"Cis_valid_path",                        (DL_FUNC) &Cis_valid_path,                         3},
    {"CkuniqueN",                             (DL_FUNC) &CkuniqueN,                              1},
    {"Cminmax",                               (DL_FUNC) &Cminmax,                                3},
    {"Cn_paths_svt0",                         (DL_FUNC) &Cn_paths_svt0,                         11},
    {"Cnames2int",                            (DL_FUNC) &Cnames2int,                             2},
    {"Cnchar",                                (DL_FUNC) &Cnchar,                                 1},
    {"count_sort_logi",                       (DL_FUNC) &count_sort_logi,                        1},
    {"CpackSum",                              (DL_FUNC) &CpackSum,                               2},
    {"Cpad0",                                 (DL_FUNC) &Cpad0,                                  2},
    {"Cpcg_hash",                             (DL_FUNC) &Cpcg_hash,                              4},
    {"Cpmax0",                                (DL_FUNC) &Cpmax0,                                 1},
    {"Crange_nchar",                          (DL_FUNC) &Crange_nchar,                           1},
    {"CShuffleRindex",                        (DL_FUNC) &CShuffleRindex,                         1},
    {"Csimulate_racf",                        (DL_FUNC) &Csimulate_racf,                        12},
    {"Csqrt2",                                (DL_FUNC) &Csqrt2,                                 1},
    {"Csum_int",                              (DL_FUNC) &Csum_int,                               1},
    {"Csum_isna",                             (DL_FUNC) &Csum_isna,                              2},
    {"CSumRaw",                               (DL_FUNC) &CSumRaw,                                2},
    {"Ctabula_RecordID",                      (DL_FUNC) &Ctabula_RecordID,                       1},
    {"Ctest_find_first",                      (DL_FUNC) &Ctest_find_first,                       3},
    {"Ctest_qru",                             (DL_FUNC) &Ctest_qru,                              5},
    {"Ctest_radix_find",                      (DL_FUNC) &Ctest_radix_find,                       3},
    {"Ctest_radix_find_range",                (DL_FUNC) &Ctest_radix_find_range,                 3},
    {"Cunique_sorted",                        (DL_FUNC) &Cunique_sorted,                         1},
    {"Cvalidate_clique",                      (DL_FUNC) &Cvalidate_clique,                       4},
    {"Cvalidate_encoding",                    (DL_FUNC) &Cvalidate_encoding,                     2},
    {"CValidate3202",                         (DL_FUNC) &CValidate3202,                          1},
    {"Cwhich_isnt_int",                       (DL_FUNC) &Cwhich_isnt_int,                        1},
    {"Cwhich_isnt_sorted",                    (DL_FUNC) &Cwhich_isnt_sorted,                     1},
    {"Cwhichminmax",                          (DL_FUNC) &Cwhichminmax,                           1},
    {"do_between",                            (DL_FUNC) &do_between,                             4},
    {"do_bsearch",                            (DL_FUNC) &do_bsearch,                             2},
    {"is_binary_call",                        (DL_FUNC) &is_binary_call,                         1},
    {"len3_paths",                            (DL_FUNC) &len3_paths,                             3},
    {"len4_paths",                            (DL_FUNC) &len4_paths,                             4},
    {"lookup4_char",                          (DL_FUNC) &lookup4_char,                           1},
    {"qgraph_layout_Cpp",                     (DL_FUNC) &qgraph_layout_Cpp,                     14},
    {"showsqrt_fast",                         (DL_FUNC) &showsqrt_fast,                          1},
    {"test_input_types",                      (DL_FUNC) &test_input_types,                       4},
    {NULL, NULL, 0}
};

void R_init_hutilsc(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
