/* spectrum.c
 * Coded by jungerstein. 
 */

#include "spectrum.h"
#include <math.h>

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
