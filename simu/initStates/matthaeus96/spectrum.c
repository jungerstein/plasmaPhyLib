/* spectrum.c
 * Coded by jungerstein. 
 */

#include "spectrum.h"
#include <math.h>
#include <common.h>

void generateDvPerpKImage(imageVec res, index ijk, int n, double d, paramMatt param)
{
  int i, j, k; 
  vec kVec, kRight; 
  double kNorm, amp; 
  vec perp; 
  fftw_complex phase; 

  i = ijk[0]; 
  j = ijk[1]; 
  k = ijk[2]; 
  kVec[0] = i * d; 
  kVec[1] = j * d; 
  kVec[2] = k * d; 
  kRight[0] = exactK(kVec[0], d); 
  kRight[1] = exactK(kVec[1], d); 
  kRight[2] = exactK(kVec[2], d); 
  kNorm = normVec(kVec); 
  amp = spectrumMatt96(kNorm, param); 
  randPerpVec(perp, kNorm); 
  phase = randPhaseFactor(); 

  res[0] = perp[0] * phase * amp; 
  res[1] = perp[1] * phase * amp; 
  res[2] = perp[2] * phase * amp; 
}

void generatePerpImages(fftw_complex * comp1, fftw_complex * comp2, fftw_complex comp3, int n, double d, paramMatt param)
{
  int i, j, k;  
  index ijk; 
  _Complex double image;
  for (k=0; k<n; k++)
  {
    ijk[2] = k;
    for (j=0; j<n; j++)
    {
      ijk[1] = j;
      for (i=0; i<n; i++)
      {
        ijk[0] = i;
        generateDvPerpKImage(image, n, d, param); 
        comp1[el(i, j, k)] = image[0]; 
        comp2[el(i, j, k)] = image[1]; 
        comp3[el(i, j, k)] = image[2]; 
      }
    }
  }
}

double spectrumMatt96(double k, paramMatt param)
{
  if (k > param.kMax || k < param.kMin)
  { 
    return 0.0;
  }

  return sqrt(1 / (1 + pow(k / param.kKnee, param.q))); 
}

void trimVec(vec trim, vec v, vec k, vec d)
{
  vec kRight; 
  double normTrim, normV; 
  normV = normVec(v); 
  if (normV < EMISIU) /* Safe, normV is bounded to >= 0. */
  {
    trim[0] = 0; 
    trim[1] = 0; 
    trim[2] = 0; 
    return; 
  }
  kRight[0] = exactK(k[0], d[0]); 
  kRight[1] = exactK(k[1], d[1]); 
  kRight[2] = exactK(k[2], d[2]); 
  schmitt(trim, v, kRight); 
  normTrim = normVec(trim); 
  if (normTrim < EMISIU) /* Yes, remove it. */
  {
    trim[0] = 0; 
    trim[1] = 0; 
    trim[2] = 0; 
    return; 
  }
  trim[0] *= (normV / normTrim); 
  trim[1] *= (normV / normTrim); 
  trim[2] *= (normV / normTrim); 
}
