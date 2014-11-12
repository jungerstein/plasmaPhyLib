/* 3dSpectrum.h
 * Header file for spectrum. */
#ifndef JSTEIN_PLASMALIB_3DSPECTRUM_H__
#define JSTEIN_PLASMALIB_3DSPECTRUM_H__

#include <complex.h>
#include <fftw3.h>

typedef int Size[3]; 
typedef double Grid[3]; 

void calPowerPara(double * power, fftw_complex * fft, int nPower, double dk, double * kk, double * dir, int * n);
void calPowerPerp(double * power, fftw_complex * fft, int nPower, double dk, double * kk, double * dir, int * n);
void fft3d(fftw_complex * out, double * k, fftw_complex * in, int * n, double * delta);
#endif
