/* phase.h
 * Random phase angle. 
 */

#ifndef JSTEIN_MATT96_PHASE_H
#define JSTEIN_MATT96_PHASE_H

_Complex double phaseFactor(double angle);
void randomAngle(_Complex double * res, double * amp, int nData); 
_Complex double randPhaseFactor(void); 

#endif /* ifndef JSTEIN_MATT96_PHASE_H */
