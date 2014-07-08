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
