#ifndef JSTEIN_MATT96_COMMON_H
#define JSTEIN_MATT96_COMMON_H

/* Use dialect reading so that it will not mix with other epsilons. */
#define EMISIU 1e-8

/* Define element, but nx, ny, nz should be defined. */
#define el(i, j, k) ((k)*ny*nx + (j)*nx + (i))

typedef int index[3];

#endif /* ifndef JSTEIN_MATT96_COMMON_H */
