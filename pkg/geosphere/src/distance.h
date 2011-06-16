
double pi();

/* Convert degrees to radians */
double toRad(double deg) ; 

/* Convert radians to degrees */
double toDeg(double rad) ; 

/* normatlize longitude between -180 .. 180 degrees*/
double normLonDeg(double lon);

/* normatlize longitude between -pi .. p1 radians*/
double normLonRad(double lon);

/* are two points antipodal */
unsigned char isAntipode(double lon1, double lat1, double lon2, double lat2, double tol);

/* law of cosines distance */
double distCos(double lon1, double lat1, double lon2, double lat2, double r) ;
