/*  
** Robert Hijmans, June 2011
**
** This program is free software; you can redistribute it and/or
** modify it under the terms of the GNU General Public License as
** published by the Free Software Foundation; either version 2 of the
** License, or (at your option) any later version.
**
** This program is distributed in the hope that it will be useful, but
** WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
** General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
**
*/

#include    <R.h>
#include	<stdio.h>
#include	<stdlib.h>
#include	<math.h>
#include	"distance.h"



void interm(int *n, double *lon1, double *lat1, double *lon2, double *lat2, int *addStartEnd, double *lonint, double *latint) {

	int i, j, nn;
	double f, d, A, B, x, y, z, lon, lat, ln1, lt1, ln2, lt2;
	
	if (*addStartEnd) {
		nn = *n + 1;
		j = 1;
		lonint[0] = *lon1;
		latint[0] = *lat1;
		lonint[nn] = *lon2;
		latint[nn] = *lat2;
	} else {
		nn = *n ;
		j = 0;
	}
	
	if (isAntipode(*lon1, *lat1, *lon2, *lat2, 0.0000000001)) {
		
		for(i=j; i < nn; i++) {
			lonint[i] = R_NaReal;
			latint[i] = R_NaReal;
		}
		
	} else if ((*lon1 == *lon2) & (*lat1 == *lat2)) {
		
		for(i=j; i < nn; i++) {
			lonint[i] = *lon1;
			latint[i] = *lat1;		
		}
		
	} else {
	
		ln1 = toRad(*lon1);
		lt1 = toRad(*lat1);
		ln2 = toRad(*lon2);
		lt2 = toRad(*lat2);
		d = distCos(*lon1, *lat1, *lon2, *lat2, 1.);

		for(i=j; i < nn; i++) {
			f = (i-j+1.) / (*n + 1.);
			A = sin(1.-f) * d / sin(d);
			B = sin(f*d) / sin(d);
			x = A * cos(lt1) * cos(ln1) + B * cos(lt2) * cos(ln2);
			y = A * cos(lt1) * sin(ln1) + B * cos(lt2) * sin(ln2);
			z = A * sin(lt1) + B*sin(lt2);
			lat = atan2(z, sqrt(x*x + y*y));
			lon = atan2(y,x);
			latint[i] = toDeg(lat);
			lonint[i] = toDeg(lon);
		}
	}
}


