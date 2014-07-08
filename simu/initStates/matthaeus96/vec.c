/* vec.c
 * Vector operations. 
 */

#include "vec.h"
#include <math.h>
#include <stdlib.h>
#include "common.h"

void crossVec(vec x, vec a, vec b)
{
  x[0] = a[1] * b[2] - a[2] * b[1]; 
  x[1] = a[2] * b[0] - a[0] * b[2]; 
  x[2] = a[0] * b[1] - a[1] * b[0]; 
}

double dotVec(vec a, vec b) 
{
  return (a[0]*b[0] + a[1]*b[1] + a[2]*b[2]); 
}

void eigVec(vec eig, vec v)
{
  double norm; 
  norm = normVec(v); 
  eig[0] = v[0] / norm;
  eig[1] = v[1] / norm;
  eig[2] = v[2] / norm;
}

double normVec(vec v)
{
  return sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2]); 
}

void randPerpVec(vec res, vec v)
{
  double th, phi; 
  vec cross, guess; 
  double normCross; 
  do 
  {
    th = rand() * 2 * M_PI / RAND_MAX; 
    phi = rand() * 2 * M_PI / RAND_MAX; 
    guess[0] = sin(th) * cos(phi); 
    guess[1] = sin(th) * sin(phi); 
    guess[2] = cos(th); 
    crossVec(cross, guess, v); 
    normCross = normVec(cross); 
  }
  while (normCross < EMISIU); 
  res[0] = cross[0] / normCross; 
  res[1] = cross[1] / normCross; 
  res[2] = cross[2] / normCross; 
}

void schmitt(vec sch, vec a, vec b)
{
  double dotProd; 
  vec bEin; 
  eigVec(bEin, b); 
  dotProd = dotVec(a, bEin); 
  sch[0] = a[0] - dotProd * bEin[0]; 
  sch[1] = a[1] - dotProd * bEin[1]; 
  sch[2] = a[2] - dotProd * bEin[2]; 
}

