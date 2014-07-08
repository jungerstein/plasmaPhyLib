/* vec.h
 * Vector operations. 
 */

#ifndef JSTEIN_C_COMMON_VEC_H
#define JSTEIN_C_COMMON_VEC_H

typedef double vec[3]; 

/* Use dialect reading so that it will not mix with other epsilons. */
#define EMISIU 1e-8

void crossVec(vec x, vec a, vec b); 
double dotVec(vec a, vec b); 
void eigVec(vec eig, vec v); 
double normVec(vec v); 
void randPerpVec(vec res, vec v); 
/* Change direciton of vec a. */
void schmitt(vec sch, vec a, vec b); 

#endif /* ifndef JSTEIN_C_COMMON_VEC_H */
