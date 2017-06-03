% $Id: functions.pl,v 1.3 2016-11-08 15:04:13-08 - - $

mathfns( X, List ) :-
   S is sin( X ),
   C is cos( X ),
   Q is sqrt( X ),
   List = [S, C, Q].

constants( List ) :-
   Pi is pi,
   E is e,
   Epsilon is epsilon,
   List = [Pi, E, Epsilon].

dms_to_dec(degmin(Degree, Minutes), Decimal) :-
   Decimal is Degree + (Minutes / 60).

degree_to_radians(Degree, Radians) :-
   Radians is ((Degree * pi) / (180)).

sincos( X, Y ) :-
   Y is sin( X ) ** 2 + cos( X ) ** 2.

haversine_radians( Lat1, Lon1, Lat2, Lon2, Distance ) :-
   Dlon is Lon2 - Lon1,
   Dlat is Lat2 - Lat1,
   A is sin( Dlat / 2 ) ** 2
      + cos( Lat1 ) * cos( Lat2 ) * sin( Dlon / 2 ) ** 2,
   Dist is 2 * atan2( sqrt( A ), sqrt( 1 - A )),
   Distance is Dist * 3961.

compute_distance(Airport1, Airport2, Distance) :-
   airport(Airport1, _, Lat1, Lon1),
   airport(Airport2, _, Lat2, Lon2),
   dms_to_dec(Lat1, Lat1_dec),
   dms_to_dec(Lon1, Lon1_dec),
   dms_to_dec(Lat2, Lat2_dec),
   dms_to_dec(Lon2, Lon2_dec),
   degree_to_radians(Lat1_dec, Lat1_rad),
   degree_to_radians(Lon1_dec, Lon1_rad),
   degree_to_radians(Lat2_dec, Lat2_rad),
   degree_to_radians(Lon2_dec, Lon2_rad),
   haversine_radians(Lat1_rad, Lon1_rad, Lat2_rad, Lon2_rad, Distance).

get_distance(Airport1, Airport2) :-
   compute_distance(Airport1, Airport2, Distance),
   write('Distance: '),
   write(Distance),
   nl.