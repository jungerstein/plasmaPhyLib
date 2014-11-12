/* 3dSpectrum.c
 * Use FFTW for 3D spectrum. 
 */

#include <math.h>
#include "3dSpectrum.h"

void fft3d(fftw_complex * out, Grid k, double * in, Size n, Grid delta)
{
  fftw_plan plan;

  plan = fftw_dft_r2c_3d(n[0], n[1], n[2], in, out, FFTW_MEASURE); 
  k[0] = 2 * M_PI / (n[0] * delta[0]); 
  k[1] = 2 * M_PI / (n[1] * delta[1]); 
  k[2] = 2 * M_PI / (n[2] * delta[2]); 
  fftw_execute(plan); 
  fftw_destroy_plan(plan); 
}

