| long1 lat1 long2 lat2 long12 d1 d2 alpha1 alpha2 
	alpha0 sigma1 sigma2 long01 long0 sigmamid 
	latmid longmid alphamid |

"valparaiso - shanghai"
lat1 := -33.
long1 := -71.6.
lat2 := 31.4.
long2 := 121.8.

long12 := long2 - long1.
long12 := long12 abs > 180
	ifTrue: [ 
		long12 > 180 
			ifTrue: [ long12 - 360 ]
			ifFalse: [ long12 + 360 ]
		 ]
	ifFalse: [
		long12
		].

lat1 := lat1 degreesToRadians.
lat2 := lat2 degreesToRadians.
long1 := long1 degreesToRadians.
long2 := long2 degreesToRadians.
long12 := long12 degreesToRadians.

d1 := lat2 cos * long12 sin.
d2 := (lat1 cos * lat2 sin) - (lat1 sin * lat2 cos * long12 cos).
alpha1 := d1 arcTan: d2.

d1 := lat1 cos * long12 sin.
d2 := (lat2 sin * lat1 cos * long12 cos) - (lat2 cos* lat1 sin).
alpha2 := d1 arcTan: d2.

d1 := alpha1 sin * lat1 cos.
d2 := ((alpha1 cos raisedTo: 2) + ((alpha1 sin raisedTo: 2)*(lat1 sin raisedTo: 2))) sqrt.
alpha0 := d1 arcTan: d2.

sigma1 := lat1 tan arcTan: alpha1 cos.

sigma2 := lat2 tan arcTan: alpha2 cos.

d1 := alpha0 sin * sigma1 sin.
d2 := sigma1 cos.
long01 := d1 arcTan: d2.
long0 := long1 - long01.
long0 radiansToDegrees.
sigmamid := (sigma1 + sigma2)/2.

d1 := alpha0 cos * sigmamid sin.
d2 := ((sigmamid cos raisedTo: 2) + ((alpha0 sin raisedTo: 2)*(sigmamid sin raisedTo: 2))) sqrt.

latmid := d1 arcTan: d2.

d1 := alpha0 sin * sigmamid sin.
d2 := sigmamid cos.
longmid := d1 arcTan: d2.
longmid := (longmid+long0).

alphamid := alpha0 tan arcTan: sigmamid cos.



