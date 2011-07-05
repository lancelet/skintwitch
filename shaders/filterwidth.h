/** Facilities for filter-width estimation. */

#ifndef FILTERWIDTH_H
#define FILTERWIDTH_H

#define MINFILTWIDTH 1.0E-6
#define filterwidth(x) max(abs(Du(x)*du) + abs(Dv(x)*dv), MINFILTWIDTH)
#define filterwidthp(p) max(sqrt(area(P)), MINFILTWIDTH)

#endif
