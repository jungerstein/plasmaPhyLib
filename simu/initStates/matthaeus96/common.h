#ifndef JSTEIN_MATT96_COMMON_H
#define JSTEIN_MATT96_COMMON_H

/* Define element, but nx, ny, nz should be defined. */
#define el(i, j, k) ((k)*ny*nx + (j)*nx + (i))

typedef int index[3];

#endif /* ifndef JSTEIN_MATT96_COMMON_H */
