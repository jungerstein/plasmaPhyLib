/* paraPerpSpectrumPlot.h
 * Plots para and perp distr of spectrum. 
 */

#ifndef JSTEIN_PLASMALIB_PARAPERP_SPECTRUM_PLOT_H__
#define JSTEIN_PLASMALIB_PARAPERP_SPECTRUM_PLOT_H__

#include <plplot.h>

void plotPSD(double * power, int nScale, char * filename, char * quan, double dk); 

/* Interior methods. */
static void copyVal(PLFLT * out, double * in, int N); 
static PLFLT maxVal(PLFLT * val, int N); 
static void setK(PLFLT * out, double dk, int N); 
static void toLog(PLFLT * out, PLFLT * in, int N); 

#endif
