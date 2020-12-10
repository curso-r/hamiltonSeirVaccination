//#include "twoagesR.h"
#include "twoagesR2.h"
#include <R_ext/Rdynload.h>

//void R_init_hamiltonSeirVaccination(DllInfo *info) {
//  R_RegisterCCallable("hamiltonSeirVaccination", "twoagesv",  (DL_FUNC) &twoages_);
//}
void R_init_hamiltonSeirVaccination(DllInfo *info) {
  R_RegisterCCallable("hamiltonSeirVaccination", "twoagesv2",  (DL_FUNC) &twoages2_);
}