#include "hutilsc.h"

#define MAX_WID 256
#define SUSCEPTIBLE_DATE 255
#define INCUBATION_PERIOD 8
#define N_COLLEAGUES 32

// Chance of being infectious on any particular day
// times UINT_MAX. Say 4/7
#define RUINT_INFECTIOUS 2454267026


int wanyOutside(const int * x, R_xlen_t N) {
  int xminmax[2] = {INT_MAX, NA_INTEGER};
  Vminmax_i(xminmax, x, N, 1);
  if (xminmax[0] != 0) {
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

typedef struct {
  int nNeighbours; // number of neighbours
  unsigned char Neighbours[MAX_WID]; // neighbours in ascending order, 0-terminated
  unsigned int * EdgeWeight;
  int n_infected;
  int n_new_infections;
} Racf;

int searchNe(unsigned char * x, int a, int n) {
  for (int i = 0; i < MAX_WID; ++i) {
    if (x[i] == a) {
      return i;
    }
  }
  return 0;
}

void insertNe(unsigned char * x, int a, int n) {
  if (x == NULL) {
    x = calloc(MAX_WID, sizeof(char));
    if (x == NULL) {
      error("insertNe unavailable."); // # nocov
    }
    x[0] = a;
    return;
  }
  int pos = searchNe(x, a, n);
  if (pos) {
    return;
  }
  int j = n - 1;
  for (; j >= pos; --j) {
    x[j + 1] = x[j];
  }
  x[j + 1] = a;
}

bool is_infectious(unsigned char infection_datei, 
                   unsigned char today, 
                   unsigned int ruint) {
  return ((today - infection_datei) < INCUBATION_PERIOD) && 
    (ruint < RUINT_INFECTIOUS);
}


SEXP Csimulate_racf(SEXP K1, SEXP K2, 
                    SEXP J1, SEXP J2,
                    SEXP M1, SEXP M2,
                    SEXP Resistance,
                    SEXP PatientZero,
                    SEXP nDays,
                    SEXP Epi,
                    SEXP nthreads) {
  // K1, K2  -- from pid to wid
  // J1, J2  -- from wid to pid
  // M1, M2 -- the positions of wid
  // E1 Raw vector, the number of individuals in contact with
  // E2 Reproduction number
  
  
  int np = 0;
  if (TYPEOF(Epi) != VECSXP) {
    error("Epi not list.");
  }
  
  if (notEquiInt2(K1, K2)) {
    error("K1,K2 not equilength integer vectors."); // # nocov
  }
  if (notEquiInt3(K1, J1, J2)) {
    error("notEquiInt3(K1, J1, J2) [E%d]", notEquiInt3(K1, J1, J2));
  }
  if (notEquiInt2(M1, M2)) {
    error("notEquiInt2(M1, M2) [E%d]", notEquiInt2(M1, M2));
  }
  int N = xlength(K1);
  if (N <= 1) {
    error("N <= 1 was unexpected.");
  }
  if (TYPEOF(Resistance) != RAWSXP || xlength(Resistance) >= INT_MAX) {
    error("Resistance was not of type RAWSXP.");
  }
  
  if (N >= (INT_MAX / 8)) {
    error("N >= INT_MAX / 8 (N = %d)", N);
  }
  int N8 = N * 8;
  
  const int * pid = INTEGER(K1); 
  const int * wid = INTEGER(K2);
  const int * wjd = INTEGER(J1);
  const int * pjd = INTEGER(J2);
  // const int * jmin = INTEGER(M1);
  // const int * jmax = INTEGER(M2);
  
  int nThread = as_nThread(nthreads);
  if (nThread > 8) {
    error("nThread > 8, not permitted for simulation.");
  }
  
  int minima[4] = {pid[0], wid[0], pjd[0], wjd[0]};
  int maxima[4] = {pid[0], wid[0], pjd[0], wjd[0]};
  char pid_sorted = pid[0] == 0;
  char wjd_sorted = wjd[0] == 0;
  
  
  // Ensure min = 0, max_wijd < MAX_WID (zero indexed and no more than wid)
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(min : minima[:4]) reduction(max : maxima[:4]) reduction(& : pid_sorted,wjd_sorted)
#endif
  for (int i = 1; i < N; ++i) {
    int pidi = pid[i];
    int pid0 = pid[i - 1];
    int widi = wid[i];
    int pjdi = pjd[i];
    int wjdi = wjd[i];
    int wjd0 = wjd[i - 1];
    pid_sorted &= pidi >= pid0;
    wjd_sorted &= wjdi >= wjd0;
    
    minima[0] = (pidi < minima[0]) ? pidi : minima[0];
    minima[1] = (widi < minima[1]) ? widi : minima[1];
    minima[2] = (pjdi < minima[2]) ? pjdi : minima[2];
    minima[3] = (wjdi < minima[3]) ? wjdi : minima[3];
    
    maxima[0] = (pidi > maxima[0]) ? pidi : maxima[0];
    maxima[1] = (widi > maxima[1]) ? widi : maxima[1];
    maxima[2] = (pjdi > maxima[2]) ? pjdi : maxima[2];
    maxima[3] = (wjdi > maxima[3]) ? wjdi : maxima[3];
  }
  if (maxima[0] == minima[0]) {
    // pid is constant
    error("There is only person in the data (max(pid) == min(pid)) so no transmission may occur."); // nocov
  }
  
  for (int i = 0; i < 4; ++i) {
    if (minima[i]) {
      error("minima[%d] = %d != 0", i, minima[i]);
    }
  }
  int n_persons = maxima[0];
  if (n_persons != maxima[2]) {
    error("maxima[0] != maxima[2]\n(%d != %d)", maxima[0], maxima[2]);
  }
  if (maxima[1] != maxima[3]) {
    error("maxima[1] != maxima[1]\n(%d != %d)", maxima[1], maxima[3]);
  }
  if (maxima[1] >= MAX_WID) {
    error("maxima[1] >= MAX_WID\n(%d >= %d)", maxima[1], MAX_WID);
  }
  int n_wid = maxima[1];
  if (!pid_sorted) {
    error("pid unsorted");
  }
  if (!wjd_sorted) {
    error("wjd unsorted");
  }
  // length(Resistance) already known to be int
  if ((length(Resistance) - 1) != n_persons) {
    error("length(Resistance) = %d, yet n_persons = %d.", length(Resistance), n_persons);
  }
  const unsigned char * resistance = RAW(Resistance);
  
  
  int n_days = asInteger(nDays);
  if (n_days <= 1 || n_days >= (SUSCEPTIBLE_DATE - 1)) {
    error("n_days = %d ; <= 1 || n_days > 250", n_days);
  }
  if (TYPEOF(PatientZero) != INTSXP) {
    error("PatientZero not INTSXP.");
  }
  R_xlen_t nPatientZero = xlength(PatientZero);
  if (nPatientZero > 8) {
    error("xlength(PatientZero) > 8");
  }
  
  int patientsZero[8] = {N};
  for (int thread = 0; (thread < 8 && thread < nPatientZero); ++thread) {
    int patientZero = INTEGER(PatientZero)[thread];
    if (patientZero < 0 || patientZero >= N) {
      error("patientZero = %d, N = %d.", patientZero, N);
    }
    patientsZero[thread] = patientZero;
  }
  
  
  unsigned int RACF_SIZE[MAX_WID] = {0};
  for (int i = 0; i < N; ++i) {
    unsigned int widi = wid[i];
    RACF_SIZE[widi] += 1;
  }
  
  
  
  // array of possible reinfections
  // based on rpois(16, 2.2/8)
  const unsigned int R16_WORKPLACE[16] = {0, 1, 0, 1, 1, 0, 0, 0,
                                          0, 0, 0, 0, 0, 0, 0, 2};
  

  bool malloc_failures[8] = {0};
  unsigned char * infection_dates = malloc(sizeof(char) * N * 8);
  nThread = 1;
  
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) schedule(static)
#endif
  for (int thread = 0; thread < nThread; ++thread) {
    
    // Used to randomize order
    int * rindex = malloc(sizeof(int) * N);
    if (rindex == NULL) {
      free(rindex);
      malloc_failures[thread] = true;
      continue;
    }
    // Set to seq_along initially
    for (int i = 0; i < N; ++i) {
      rindex[i] = i;
    }
    
    int patientZero = patientsZero[thread];
    unsigned char * infection_date = malloc(sizeof(char) * N);
    if (infection_date == NULL) {
      free(rindex);
      free(infection_date);
      malloc_failures[thread] = true;
      continue;
    }
    for (int i = 0; i < N; ++i) {
      infection_date[i] = SUSCEPTIBLE_DATE;
    }
    infection_date[patientZero] = 0;
    
    int RACF_INFECTED[MAX_WID] = {0};
    int racf_zero = wid[patientZero];
    RACF_INFECTED[racf_zero] = 1;
    int n_infected = 1;
    for (unsigned int day = 1; day <= n_days; ++day) {
      // each day
      // calculate the number of new infections at each RACF
      // then cycle through each person until infections exhausted
      
      // These should be unsigned in case we get (unphysical)
      // explosive transmission
      unsigned short int RACF_NEW_INFECTIONS[256] = {0};
      unsigned short int n_new_infections = 0;
      // internal infections
      bool no_one_infected = true; // for early return
      for (int i = 0; i < N; ++i) {
        if (infection_date[i] == SUSCEPTIBLE_DATE) {
          continue;
        }
        no_one_infected = false;
        int pidi = pid[i];
        int widi = wid[i];
        unsigned char days_since_infection = day - infection_date[i];
        // if at end of incubation period, no longer infectious
        if (days_since_infection == INCUBATION_PERIOD) {
          RACF_INFECTED[widi] -= 1;
          int pidi = pid[i];
          int i_down = i;
          // decrement adjacent wid
          while (++i_down < N && pid[i_down] == pidi && RACF_INFECTED[wid[i_down]]) {
            RACF_INFECTED[wid[i_down]] -= 1;
          }
          if (n_infected) {
            n_infected -= 1;
          }
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
      
      if (no_one_infected) {
        break;
      }
      // shuffle rindex
      if ((day % 7u) == 3) {
        for (int i = 0; i < N - 1; ++i) {
          int j = i + tpcg_sample_halfmax(thread) / ((1073741824u) / (N - i) + 1u);
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
        int pidi = pid[i];
        int widi = wid[i];
        
        if (RACF_NEW_INFECTIONS[widi] && infection_date[i] == SUSCEPTIBLE_DATE) {
          // if resistant (random variable based on k, which is random here)
          // Use < because we need instances of 100% vaccination
          // Resistance means the infection is spent -- we don't just search 
          // for someone
          unsigned char P = tpcg_sample1c(thread);
          
          if (resistance[pidi] < P) {
            infection_date[i] = day;
          }
          RACF_NEW_INFECTIONS[widi] -= 1;
          --n_new_infections;
        }
      }
    }
    const int start = N * thread;
    for (int i = 0; i < N; ++i) {
      infection_dates[start + i] = infection_date[i];
    }
    free(rindex);
    free(infection_date);
  }
  for (int thread = 0; thread < 8; ++thread) {
    if (malloc_failures[thread]) {
      warning("malloc_failures[%d]", thread);
    }
  }
  
  
  SEXP ans = PROTECT(allocVector(RAWSXP, N8)); np++;
  unsigned char * restrict ansp = RAW(ans);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) 
#endif
  for (int i = 0; i < N8; ++i) {
    ansp[i] = infection_dates[i];
  }
  free(infection_dates);
  UNPROTECT(np);
  return ans;
}





