#include "hutilsc.h"

#define MAX_WID 256
#define SUSCEPTIBLE_DATE 255
#define INCUBATION_PERIOD 8
#define N_COLLEAGUES 32
#define MAX_NTHREAD 8

// unsigned chars for in-thread errors handled gracefully
#define WIDJ_SMALL 1

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





bool q_ru(unsigned int p, unsigned int q, unsigned int r) {
  // p/q > r/2^32
  uint64_t rq = r;
  rq *= q;
  rq >>= 32;
  return p > rq;
}
bool q_ru1(unsigned int p, unsigned int q, unsigned int r) {
  // p/q > r/2^32
  double p_q = ((double)p) / ((double)q);
  double r__ = ((double)r) / ((double)4294967296);
  return p_q > r__;
}

SEXP Ctest_qru(SEXP P, SEXP Q, SEXP M, SEXP Z, SEXP N) {
  unsigned int p = asInteger(P),  
    q = asInteger(Q),
    z = asInteger(Z);
  int m = asInteger(M);
  int n = asInteger(N);
  int o = 0;
  if (m == NA_INTEGER) {
    // test overhead
    for (int i = 0; i < n; ++i) {
      o = (int)pcg_hash(i + z);
    }
  } else if (m) {
    for (int i = 0; i < n; ++i) {
      o += q_ru1(p, q, pcg_hash(i + z));
    }
  } else {
    for (int i = 0; i < n; ++i) {
      o += q_ru(p, q, pcg_hash(i + z));
    }
  }
  return ScalarInteger(o);
}



//' @noRd
//' @name wjd_indices
//' @param idx An array of *already established* length == n_infections
//' @param wdji  The RACF in question.
//' @param n_infections The number of infections to arrange
//' @param m1,m2 pointers to the start and finish (inclusive) of each wjd
//' so m1[wjdi] <= i <= m2[wjdi]  ==>  wjd[i] == wjdi;
//' @param m1_len The length of the vectors pointed at by m1,m2.
//' @param wsize The size of the racf wjdi.
//' @param thread The current thread (passed to RNG).
//' 

void sow_wjd_indices(int * idx, 
                     int wjdi,
                     unsigned int n_infections, 
                     const int * m1, 
                     const int * m2,
                     int m1_len,
                     unsigned int wsize, 
                     int thread) {
  // amend idx so that it is sample
  int j_min = m1[wjdi];
  int j_max = m2[wjdi];
  
  // allocate
  int n_unalloc_infections = n_infections;
  int i = 0; // indexof idx
  for (int j = j_min; j <= j_max; ++j) {
    if (n_unalloc_infections > 0 && q_ru(n_infections, wsize, trand_pcg(thread))) {
      idx[i] = j;
      ++i;
      --n_unalloc_infections;
    }
  }
  // if still unallocated, make one and only more pass
  if (n_unalloc_infections > 0) {
    for (int j = j_min; j <= j_max; ++j) {
      if (n_unalloc_infections > 0 && q_ru(n_infections, wsize, trand_pcg(thread))) {
        idx[i] = j;
        ++i;
        --n_unalloc_infections;
      }
    }
  }
}

bool is_infectious(unsigned char infection_datei, 
                   unsigned char today, 
                   unsigned int ruint) {
  return ((today - infection_datei) < INCUBATION_PERIOD) && 
    (ruint < RUINT_INFECTIOUS);
}

bool is_resistant(int pidi, 
                  const unsigned char * resistance, 
                  unsigned char r) {
  return resistance[pidi] >= r;
}

bool isnt_susceptible(int pidi, 
                      unsigned char * infection_date,
                      const unsigned char * resistance,
                      unsigned char r) {
  return (infection_date[pidi] == SUSCEPTIBLE_DATE) ||
    is_resistant(pidi, resistance, r);
  
}


void do_simulate(int thread,
                 int n_days,
                 int * patientsZero, 
                 int n_persons,
                 bool malloc_failures[MAX_NTHREAD],
                 unsigned char misc_failure[MAX_NTHREAD],
                 const int * pid,
                 const int * wid, 
                 const int * pjd,
                 const int * wjd,
                 int N,
                 const int * R16_WORKPLACE,
                 const int * RACF_SIZE,
                 const unsigned char * resistance,
                 unsigned char * infection_dates,
                 const int * m1,
                 const int * m2,
                 int m1_len) {
  int patientZero = patientsZero[thread];
  unsigned char * infection_date = malloc(sizeof(char) * n_persons);
  if (infection_date == NULL) {
    free(infection_date);
    malloc_failures[thread] = true;
    return;
  }
  for (int i = 0; i < n_persons; ++i) {
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
      int pidi = pid[i];
      unsigned char pid_infection_date = infection_date[pidi];
      if (pid_infection_date == SUSCEPTIBLE_DATE) {
        continue;
      }
      no_one_infected = false;
      
      int widi = wid[i];
      unsigned char days_since_infection = day - pid_infection_date;
      // if at end of incubation period, no longer infectious
      if (days_since_infection == INCUBATION_PERIOD) {
        RACF_INFECTED[widi] -= 1;
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
    
    // Loop through each RACF. If infected,
    for (int widj = 0; widj < MAX_WID; ++widj) {
      int n_infections = RACF_NEW_INFECTIONS[widj] & 31;
      if (n_infections == 0) {
        continue;
      }
      int widj_size = RACF_SIZE[widj];
      if (widj_size < n_infections) {
        // number of infections exceeded
        misc_failure[thread] = WIDJ_SMALL;
        continue;
      }
      // so widj needs n_infections allocated
      int idj[32] = {0};
      sow_wjd_indices(idj, widj, n_infections, m1, m2, m1_len, widj_size, thread);
      for (int k = 0; k < n_infections; ++k) {
        int j = idj[k];
        int pidj = pjd[j];
        if (is_resistant(pidj, resistance, tpcg_sample1c(thread))) {
          continue;
        }
        infection_date[pidj] = day;
      }
    }
  }
  const int start = N * thread;
  for (int i = 0; i < N; ++i) {
    infection_dates[start + i] = infection_date[i];
  }
  free(infection_date);
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
  const int * m1 = INTEGER(M1);
  const int * m2 = INTEGER(M2);
  int m1_len = length(M1);
  
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
  const unsigned int nPatientZero = xlength(PatientZero);
  const int * patientsZero = INTEGER(PatientZero);
  
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
  unsigned char misc_failure[8] = {0};
  const unsigned int N_ans = nPatientZero * n_persons;
  Rprintf("N_ans = %u\n", N_ans);
  SEXP ans = PROTECT(allocVector(RAWSXP, N_ans)); np++;
  unsigned char * infection_dates = RAW(ans);
  nThread = 8;
  
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread)
#endif
  for (unsigned int i = 0; i < N_ans; ++i) {
    infection_dates[i] = SUSCEPTIBLE_DATE;
  }

  
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) schedule(dynamic)
#endif
  for (int pp = 0; pp < nPatientZero; ++pp) {
    int patientZero = patientsZero[pp];
    const int start = n_persons * pp; // offset for index into infection_dates,
    int thread = 0;
#if defined _OPENMP && _OPENMP >= 201511
    thread = omp_get_thread_num() & 7;
#endif
    for (int i = 0; i < n_persons; ++i) {
      infection_dates[start + i] = SUSCEPTIBLE_DATE;
    }
    
    infection_dates[start + patientZero] = 0;
    int RACF_INFECTED[MAX_WID] = {0};
    // Show the first RACF as infected
    int which_pid_is_zero = binary_find(patientZero, (int *)pid, N);
    if (which_pid_is_zero >= 0) {
      while (pid[which_pid_is_zero] && which_pid_is_zero < N) {
        RACF_INFECTED[wid[which_pid_is_zero]] += 1;
        ++which_pid_is_zero;
      }
    }
    int n_infected = 1;
    for (unsigned int day = 1; day <= n_days; ++day) {
      // each day
      // calculate the number of new infections at each RACF
      // then cycle through each person until infections exhausted
      
      // These should be unsigned in case we get (unphysical)
      // explosive transmission
      unsigned short int RACF_NEW_INFECTIONS[MAX_WID] = {0};
      unsigned short int n_new_infections = 0;
      // internal infections
      bool no_one_infected = true; // for early return
      for (int i = 0; i < N; ++i) {
        int pidi = pid[i];
        unsigned int ruint = trand_pcg(thread);
        unsigned char pid_infection_date = infection_dates[start + pidi];
        if (pid_infection_date == SUSCEPTIBLE_DATE) {
          continue;
        }
        no_one_infected = false;
        
        int widi = wid[i];
        unsigned char days_since_infection = day - pid_infection_date;
        // if at end of incubation period, no longer infectious
        if (days_since_infection == INCUBATION_PERIOD) {
          RACF_INFECTED[widi] -= 1;
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
      
      // Loop through each RACF. If infected,
      for (int widj = 0; widj < MAX_WID; ++widj) {
        int n_infections = RACF_NEW_INFECTIONS[widj] & 31;
        if (n_infections == 0) {
          continue;
        }
        int widj_size = RACF_SIZE[widj];
        if (widj_size < n_infections) {
          // number of infections exceeded
          misc_failure[thread] = WIDJ_SMALL;
          continue;
        }
        // so widj needs n_infections allocated
        int idj[32] = {0};
        sow_wjd_indices(idj, widj, n_infections, m1, m2, m1_len, widj_size, thread);
        for (int k = 0; k < n_infections; ++k) {
          int j = idj[k];
          int pidj = pjd[j];
          if (is_resistant(pidj, resistance, tpcg_sample1c(thread))) {
            continue;
          }
          infection_dates[start + pidj] = day;
        }
      }
      
      
    }
  }
  for (int thread = 0; thread < 8; ++thread) {
    if (malloc_failures[thread]) {
      warning("malloc_failures[%d]", thread);
    }
    if (misc_failure[thread] != 0) {
      warning("misc_failure[%d] = '%c'.", thread, misc_failure[thread]);
    }
  }
  UNPROTECT(np);
  return ans;
}





