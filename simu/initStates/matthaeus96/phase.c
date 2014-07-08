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

_Complex double randomAngle(double amp)
{
  return (amp * randPhaseFactor()); 
}
