/*  
** R interface to magfield.c 
** Robert Hijmans, June 2011
** Based on the C command line interface by Ed Willians
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
#include    "magfield.h"


void magfield(int *n, double *lon, double *lat, double *h, unsigned long *julday, int*model, double *result) 
{
	double dec;
	double field[6];
	int i;
	
/*	unsigned long julian; for 1950-2049 only
	julian = yymmdd_to_julian_days(*yy, *mm, *dd); */
	
	for(i=0; i < *n; i++) {
		dec = rad_to_deg( SGMagVar( deg_to_rad(lat[i]), deg_to_rad(lon[i]), h[i], julday[i], model[i], field) );
		result[i*5+0] = dec;
		result[i*5+1] = rad_to_deg(atan(field[5]/pow(field[3]*field[3]+field[4]*field[4],0.5)));
		result[i*5+2] = field[3];
		result[i*5+3] = field[4];
		result[i*5+4] = field[5];
	}
  /* output variation (E positive),  dip angle (down positive), N, E, down components of B (nTesla) */
}
  
  

