/* 3dSpectrum.c
 * Use FFTW for 3D spectrum. 
 */

#include <math.h>
#include <stdlib.h>
#include "3dSpectrum.h"

void calPowerPara(double * power, fftw_complex * fft, int nPower, double dk, double * kk, double * dir, int * n)
{

  for (int iPower = 0; iPower < nPower; iPower++)
  {
    power[iPower] = 0; 
  }

  double kPara;
  int kBin;
  fftw_complex * p; 
  fftw_complex coefFFT;
  p = fft;
  for (int k = 0; k < n[2]; k++) 
  for (int j = 0; j < n[1]; j++) 
  for (int i = 0; i < n[0]; i++) 
  {
    kPara = fabs(i*kk[0]*dir[0] + j*kk[1]*dir[1] + k*kk[2]*dir[2]); 
    kBin = ceil(kPara/dk - 0.5); 
    if (kBin < 0) kBin = 0;
    if (kBin >= nPower) kBin = nPower;
    coefFFT = *p++; 
    if (i <= n[0]/2 && j <= n[1]/2 && k <= n[2]/2)
    {
      coefFFT *= 1 + (i+j+k != 0);
      power[kBin] += cabs(coefFFT) * cabs(coefFFT); 
    }
  }
}

void calPowerPerp(double * power, fftw_complex * fft, int nPower, double dk, double * kk, double * dir, int * n)
{
  fftw_complex * p; 
  fftw_complex coefFFT;
  p = fft;

  for (int iPower = 0; iPower < nPower; iPower++)
  {
    power[iPower] = 0; 
  }

  double kPerp; 
  int kBin;
  double xPara, yPara, zPara;
  double xRemn, yRemn, zRemn;
  for (int k = 0; k < n[2]; k++) 
  for (int j = 0; j < n[1]; j++) 
  for (int i = 0; i < n[0]; i++) 
  {
    xPara = i*kk[0]*dir[0]; 
    yPara = j*kk[1]*dir[1]; 
    zPara = k*kk[2]*dir[2]; 
    xRemn = i*kk[0] - xPara;
    yRemn = j*kk[1] - yPara;
    zRemn = k*kk[2] - zPara;
    kPerp = sqrt(xRemn*xRemn + yRemn*yRemn + zRemn*zRemn); 

    kBin = ceil(kPerp/dk - 0.5); 
    if (kBin >= nPower) kBin = nPower;
    coefFFT = *p++; 
    if (i <= n[0]/2 && j <= n[1]/2 && k <= n[2]/2)
    {
      coefFFT *= 1 + (i+j+k != 0);
      power[kBin] += cabs(coefFFT) * cabs(coefFFT); 
    }
  }
}

void fft3d(fftw_complex * out, double * k, fftw_complex * in, int * n, double * delta)
{
  fftw_plan plan;

  plan = fftw_plan_dft_3d(n[0], n[1], n[2], in, out, FFTW_FORWARD, FFTW_ESTIMATE); 
  k[0] = 2 * M_PI / (n[0] * delta[0]); 
  k[1] = 2 * M_PI / (n[1] * delta[1]); 
  k[2] = 2 * M_PI / (n[2] * delta[2]); 
  fftw_execute(plan); 
  fftw_destroy_plan(plan); 

  for (int i = 0; i < n[0]*n[1]*n[2]; i++)
  {
    out[i] /= (n[0]*n[1]*n[2]);
  }
}


