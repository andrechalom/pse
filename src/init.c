#include <stdlib.h> 
#include <R_ext/Rdynload.h>
#include <Rinternals.h>

void corcorr (double *vars, double *cor, int * N, int * M, int * l, int * FLAGSTOP); 

/* Registering native routine for R 3.4.0 */
static R_NativePrimitiveArgType C_types[] = {
    REALSXP, REALSXP, INTSXP, INTSXP, INTSXP, INTSXP
};

static const R_CMethodDef cMethods[] = {
   {"corcorr", (DL_FUNC) &corcorr, 6, C_types},
   {NULL, NULL, 0, NULL}
};

void R_init_pse(DllInfo *dll) {
   R_registerRoutines(dll, cMethods, NULL, NULL, NULL);
     R_useDynamicSymbols(dll, FALSE);
     R_forceSymbols(dll, TRUE);
}
