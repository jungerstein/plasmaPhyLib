/* spectrum.h
 * Header file and definition for specturm definition and wave packet
 *   specifications. 
 * Coded by jungerstein. 
 */
#ifndef JSTEIN_MATT96_SPECTRUM_H
#define JSTEIN_MATT96_SPECTRUM_H

typedef struct
{
  double kMin; 
  double kMax;
  double kKnee; 
  double q;
}
paramMatt;

typedef double vec[3]; 
typedef int index[3];

/* Gives amplitude of phys (instead of PSD). */
double spectrumMatt96(double k, paramMatt param); 

#endif /* ifndef JSTEIN_MATT96_SPECTRUM_H */
