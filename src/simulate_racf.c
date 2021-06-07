#include "hutilsc.h"

int wanyOutside(const int * x, R_xlen_t N) {
  int xminmax[2] = {INT_MAX, NA_INTEGER};
  Vminmax_i(xminmax, x, N, 1);
  if (xminmax[0] != 1) {
    return 1;
  }
  if (xminmax[1] >= 255) {
    return 2;
  }
  return 0;
}

SEXP CShuffleRindex(SEXP Rindex) {
  if (TYPEOF(Rindex) != INTSXP || xlength(Rindex) >= INT_MAX) {
    return R_NilValue;
  }
  int N = xlength(Rindex);
  SEXP Rindex2 = PROTECT(Rf_duplicate(Rindex));
  int * restrict rindex = INTEGER(Rindex2);
  for (int i = 0; i < N - 1; ++i) {
    int j = i + pcg_sample_halfmax() / (1073741824u / (N - i) + 1u);
    int t = rindex[j];
    rindex[j] = rindex[i];
    rindex[i] = t;
  }
  UNPROTECT(1);
  return Rindex2;
}



SEXP Csimulate_racf(SEXP K1, SEXP K2, SEXP WW, SEXP PatientZero,
                    SEXP nDays,
                    SEXP Epi) {
  int np = 0;
  if (TYPEOF(Epi) != VECSXP) {
    error("Epi not list.");
  }
  
  if (notEquiInt3(K1, K2, WW)) {
    error("K1,K2,WW not equilength integer vectors."); // # nocov
  }
  int N = xlength(K1);
  const int * pid = INTEGER(K1);
  const int * wid = INTEGER(K2);
  if (wanyOutside(wid, N)) {
    error("wid exceeds 255.");
  }
  int patientZero = asInteger(PatientZero) - 1;
  if (patientZero < 0 || patientZero >= N) {
    error("patientZero = %d, N = %d.", patientZero, N);
  }
  int n_days = asInteger(nDays);
  if (n_days <= 1) {
    error("n_days <= 1");
  }
  SEXP DateInfected = PROTECT(allocVector(INTSXP, N)); 
  np++;
  int * infection_date = INTEGER(DateInfected);
  for (int i = 0; i < N; ++i) {
    infection_date[i] = INT_MAX;
  }
  infection_date[patientZero] = 0;
  
  unsigned int RACF_SIZE[256] = {0};
  for (int i = 0; i < N; ++i) {
    unsigned int widi = wid[i];
    RACF_SIZE[widi] += 1;
  }
  
  
  int RACF_INFECTED[256] = {0};
  
  int racf_zero = wid[patientZero];
  RACF_INFECTED[racf_zero] = 1;
  
  // array of possible reinfections
  // based on rpois(16, 2.2/8)
  const unsigned int R16_WORKPLACE[16] = {0, 1, 0, 1, 1, 0, 0, 0,
                                          0, 0, 0, 0, 0, 0, 0, 2};
  
  const unsigned int INCUBATION_PERIOD = 8u;
  int n_infected = 1;
  
  // Used to randomize order
  SEXP Rindex = PROTECT(allocVector(INTSXP, N)); np++;
  int * rindex = INTEGER(Rindex);
  // Set to seq_along initially
  for (int i = 0; i < N; ++i) {
    rindex[i] = i;
  }
  
  for (int thread = 0; thread < 1; ++thread) {
    for (unsigned int day = 1; day <= n_days; ++day) {
      int RACF_NEW_INFECTIONS[256] = {0};
      int n_new_infections = 0;
      // internal infections
      for (int i = 0; i < N; ++i) {
        if (infection_date[i] == INT_MAX) {
          continue;
        }
        unsigned int widi = wid[i];
        unsigned int days_since_infection = day - infection_date[i];
        // if at end of incubation period, no longer infectious
        if (days_since_infection == INCUBATION_PERIOD) {
          RACF_INFECTED[widi] -= 1;
          int pidi = pid[i];
          int i_down = i;
          // decrement same wid
          while (++i_down < N && pid[i_down] == pidi) {
            RACF_INFECTED[wid[i_down]] -= 1;
          }
          n_infected -= 1;
          continue;
        }
        if (days_since_infection < INCUBATION_PERIOD) {
          unsigned int R16i = pcg_sample1(16u);
          // infect others by R factor
          // never more than 15 a day(!!)
          int new_infections = (R16_WORKPLACE[R16i] * RACF_INFECTED[widi]) & 15u;
          RACF_INFECTED[widi] += new_infections;
          RACF_NEW_INFECTIONS[widi] += new_infections;
          n_new_infections += new_infections;
          n_infected += new_infections;
        }
      }
      // shuffle rindex
      if ((day % 7u) == 3) {
        for (int i = 0; i < N - 1; ++i) {
          int j = i + pcg_sample_halfmax() / ((1073741824u) / (N - i) + 1u);
          int t = rindex[j];
          rindex[j] = rindex[i];
          rindex[i] = t;
        }
      }
      
      // Now infect others by RACF
      for (int k = 0; k < N; ++k) {
        if (n_new_infections <= 0) {
          break;
        }
        int i = rindex[k];
        
        unsigned int widi = wid[i];
        
        if (RACF_NEW_INFECTIONS[widi] && infection_date[i] == INT_MAX) {
          
          infection_date[i] = day;
          RACF_NEW_INFECTIONS[widi] -= 1;
          --n_new_infections;
        }
      }
    }
  }
  
  UNPROTECT(np);
  return DateInfected;
}
