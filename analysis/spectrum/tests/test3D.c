#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "3dSpectrum.h"
#define SIZE 4 

int main(void)
{
  fftw_complex phys[SIZE*SIZE*SIZE]; 
  int groesse[] = {SIZE, SIZE, SIZE};
  double delta[] = {0.2, 0.2, 0.2}; 
  double kk[3]; 
  
  fftw_complex *out; 
  out = (fftw_complex *)malloc(SIZE*SIZE*SIZE*sizeof(fftw_complex)); 

  fftw_complex *p; 
  p = phys; 
  double lambda = 0.4; 
  double lambda2y = 0.4; 
  double lambda2z = 0.4; 

  for (int k = 0; k < SIZE; k++) 
  for (int j = 0; j < SIZE; j++) 
  for (int i = 0; i < SIZE; i++) 
  {
    double x, y, z;
    x = i * delta[0];
    y = j * delta[1];
    z = k * delta[2];
    *p++ = 2 + cos(2 * M_PI / lambda * x) +
      3 * cos(2 * M_PI / lambda2y * y + 2 * M_PI / lambda2z * z); 
  }

  fft3d(out, kk, phys, groesse, delta); 

  fftw_complex *r;
  r = out;
  printf("%f %f %f\n\n", kk[0], kk[1], kk[2]); 
  for (int k = 0; k < SIZE; k++) 
  for (int j = 0; j < SIZE; j++) 
  for (int i = 0; i < SIZE; i++) 
  {
    double kx = i * kk[0];
    double ky = j * kk[1];
    double kz = k * kk[2];
    printf("k = (%f, %f, %f), fft = %g + %gi\n", kx, ky, kz, *r++); 
  }
  free(out); 

  return 0;
}
