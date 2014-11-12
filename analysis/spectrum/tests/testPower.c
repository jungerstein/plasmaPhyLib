#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "3dSpectrum.h"
#define SIZE 32

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
  double lambda = 0.8; 
  double lambda2y = 0.8; 
  double lambda2z = 0.8; 

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

  double powerPara[SIZE * 2], powerPerp[SIZE * 2]; 
  double dk = kk[0]; 
  double dir[] = {1, 0, 0}; 
  calPowerPara(powerPara, out, SIZE*2, dk, kk, dir, groesse); 
  calPowerPerp(powerPerp, out, SIZE*2, dk, kk, dir, groesse); 

  printf("\n I PARA PERP\n");
  for (int i = 0; i < SIZE; i++)
  {
    if (powerPara[i] < 1e-20) powerPara[i] = 0;
    if (powerPerp[i] < 1e-20) powerPerp[i] = 0;
    printf("%d %g %g\n", i, powerPara[i], powerPerp[i]); 
  }

  printf("\n"); 
  double sumPower, sumParaPower, sumPerpPower;
  sumPower = sumParaPower = sumPerpPower = 0;
  for (int i = 0; i < SIZE*SIZE*SIZE; i++)
  {
    sumPower += cabs(phys[i]) * cabs(phys[i]); 
  }
  for (int i = 0; i < SIZE*2; i++)
  {
    sumParaPower += powerPara[i]; 
    sumPerpPower += powerPerp[i]; 
  }
  printf("Power (orig = %g, para = %g, perp = %g) \n", 
      sumPower / (SIZE*SIZE*SIZE), sumParaPower, sumPerpPower); 

  free(out); 

  return 0;
}
