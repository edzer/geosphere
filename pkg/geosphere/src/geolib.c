#include <stdio.h>
#include "geodesic.h"

#include <R.h>
#include <Rinternals.h>
#include <stdlib.h>
#include <math.h>
#include "Rdefines.h"
#include "R_ext/Rdynload.h"
#include "Rmath.h"



/* Robert Hijmans, October 2011 */

SEXP polygonarea(SEXP latitude, SEXP longitude) {

  PROTECT(latitude = coerceVector(latitude, REALSXP));
  PROTECT(longitude = coerceVector(longitude, REALSXP));
  double *lat, *lon, *xr;
  lat = REAL(latitude);
  lon = REAL(longitude);
 
  double a = 6378137, f = 1/298.257223563; /* WGS84 */
  double A, P;
  int n, i;
  struct geod_geodesic g;
  struct geod_polygon p;
  
  geod_init(&g, a, f);
  geod_polygon_init(&p, 0);

  for (i=0; i<length(latitude); i++) {
    geod_polygon_addpoint(&g, &p, lon[i], lat[i]);
  }
  
  n = geod_polygon_compute(&g, &p, 0, 1, &A, &P);
  
  SEXP r;
  PROTECT( r = allocVector(REALSXP, 3) );
  xr = REAL(r);  
  xr[0] = n;
  xr[1] = P;
  xr[2] = A;

  UNPROTECT(3);
  return(r);  
}
