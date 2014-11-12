/* 3dSpectrum.h
 * Header file for spectrum. */
#ifndef JSTEIN_PLASMALIB_3DSPECTRUM_H__
#define JSTEIN_PLASMALIB_3DSPECTRUM_H__

#include <fftw3.h>

typedef int Size[3]; 
typedef double Grid[3]; 

void fft3d(fftw_complex * out, Grid k, double * in, Size n, Grid delta);
#endif
