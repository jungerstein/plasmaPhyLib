/* vec.c
 * Vector operations. 
 */

#include "vec.h"
#include "math.h"

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
