/* paraPerpSpectrumPlot.c
 * Plots para and perp distr of spectrum. 
 */

#include <math.h>
#include "paraPerpSpectrumPlot.h"

void plotPSD(double * power, int nScale, char * filename, char * quan, double dk)
{
  PLFLT x[nScale - 1]; // Everything we skip the DC component. 
  PLFLT y[nScale - 1]; 
  PLFLT toPlotX[nScale - 1], toPlotY[nScale - 1]; 
  PLFLT maxY;
  copyVal(y, power + 1, nScale - 1); // Skip the DC component. 
  setK(x, dk, nScale - 1); 
  toLog(toPlotX, x, nScale - 1); 
  toLog(toPlotY, y, nScale - 1); 
  maxY = maxVal(toPlotY, nScale - 1); 
  plsdev("pngcairo"); 
  plsfnam(filename); 
  plinit(); 
  plenv(toPlotX[0], toPlotX[nScale - 1 - 1], maxY - 5, maxY, 0, 30); 
  pllab("x", "y", "title"); 
  plline(nScale - 1, toPlotX, toPlotY); 
  plend(); 
}

static void copyVal(PLFLT * out, double * in, int N)
{
  for (int i = 0; i < N; i++)
  {
    out[i] = in[i]; 
  }
}

static PLFLT maxVal(PLFLT * val, int N) 
{
  PLFLT tempMax = *val;  
  for (int i = 0; i < N; i++)
  {
    if (val[i] > tempMax)
    {
      tempMax = val[i]; 
    }
  }
  return tempMax;
}

static void setK(PLFLT * out, double dk, int N)
{
  for (int i = 1; i <= N; i++)
  {
    out[i-1] = dk * i; 
  }
}

static void toLog(PLFLT * out, PLFLT * in, int N)
{
#define EPS_LOG 1e-20
  for (int i = 0; i < N; i++) 
  {
    out[i] = log10(in[i] + EPS_LOG);
  }
#undef EPS_LOG
}
