#include "twoagesR.h"
#include <R_ext/Rdynload.h>

void R_init_hamiltonSeirVaccination(DllInfo *info) {
  R_RegisterCCallable("hamiltonSeirVaccination", "twoagesv",  (DL_FUNC) &twoages_);
}