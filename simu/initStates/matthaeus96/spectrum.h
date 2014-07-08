/* spectrum.h
 * Header file and definition for specturm definition and wave packet
 *   specifications. 
 * Coded by jungerstein. 
 */
#ifndef JSTEIN_MATT96_SPECTRUM_H
#define JSTEIN_MATT96_SPECTRUM_H

#include "vec.h"
#include <fftw3.h>

typedef fftw_complex imageVec[3]; 

typedef struct
{
  double kMin; 
  double kMax;
  double kKnee; 
  double q;
}
paramMatt;

/* Gives k of higher order. Here k should be unterstood as a component.  */
/* Why C does not have a power operator???!!! */
#define exactK(k, d) ((k) * (1 - (k)*(k) * (d)*(d)/6 + (k)*(k)*(k)*(k) * (d)*(d)*(d)*(d)/120))

void generateDvPerpKImage(_Complex double * image, int n, double d, paramMatt param); 
void generatePerpImages(fftw_complex * comp1, fftw_complex * comp2, fftw_complex comp3, int n, double d, paramMatt param); 

/* Gives amplitude of phys (instead of PSD). */
double spectrumMatt96(double k, paramMatt param); 

/* ``Trims'' delta v so that delta v . k = 0 */
void trimVec(vec trim, vec v, vec k, vec d); 

#endif /* ifndef JSTEIN_MATT96_SPECTRUM_H */
