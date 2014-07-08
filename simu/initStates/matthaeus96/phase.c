/* phase.c 
 * Phase angle. 
 */

#include "phase.h"
#include <math.h>
#include <complex.h>
#include <stdlib.h>

_Complex double phaseFactor(double angle)
{
  return cos(angle) + sin(angle) * I; 
}

_Complex double randPhaseFactor(void)
{
  double alpha; 
  alpha = 2 * M_PI / RAND_MAX * rand(); 
  return phaseFactor(alpha); 
}

void randomAngle(_Complex double * res, double * amp, int nData)
{
  int i; 
  _Complex double * pOut; 
  double * pIn; 
  pOut = res; 
  pIn = amp;
  for (i = 0; i < nData; i++)
  {
    (*pOut) = randPhaseFactor() * (*pIn); 
    pOut++; 
    pIn++; 
  }
}
