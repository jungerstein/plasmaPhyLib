/* phase.c 
 * Phase angle. 
 */

#include "phase.h"
#include <math.h>
#include <complex.h>

_Complex double phaseFactor(double angle)
{
  return cos(angle) + sin(angle) * I; 
}
