% $Id: functions.pl,v 1.3 2016-11-08 15:04:13-08 - - $

not(X) :- X, !, fail.
not(_).

constants( List ) :-
   Pi is pi,
   E is e,
   Epsilon is epsilon,
   List = [Pi, E, Epsilon].

sincos( X, Y ) :-
   Y is sin( X ) ** 2 + cos( X ) ** 2.

dms_to_dec(degmin(Degree, Minutes), Decimal) :-
   Decimal is Degree + (Minutes / 60).

degree_to_radians(Degree, Radians) :-
   Radians is ((Degree * pi) / (180)).

hm_to_h(time(H, M), Hours) :-
    Hours is H + M / 60.

haversine_radians( Lat1, Lon1, Lat2, Lon2, Distance ) :-
   Dlon is Lon2 - Lon1,
   Dlat is Lat2 - Lat1,
   A is sin( Dlat / 2 ) ** 2
      + cos( Lat1 ) * cos( Lat2 ) * sin( Dlon / 2 ) ** 2,
   Dist is 2 * atan2( sqrt( A ), sqrt( 1 - A )),
   Distance is Dist * 3961.

compute_distance(Src, Dest, Distance) :-
   airport(Src, _, Lat1, Lon1),
   airport(Dest, _, Lat2, Lon2),
   dms_to_dec(Lat1, Lat1_dec),
   dms_to_dec(Lon1, Lon1_dec),
   dms_to_dec(Lat2, Lat2_dec),
   dms_to_dec(Lon2, Lon2_dec),
   degree_to_radians(Lat1_dec, Lat1_rad),
   degree_to_radians(Lon1_dec, Lon1_rad),
   degree_to_radians(Lat2_dec, Lat2_rad),
   degree_to_radians(Lon2_dec, Lon2_rad),
   haversine_radians(Lat1_rad, Lon1_rad, Lat2_rad, Lon2_rad, Distance).

compute_arrival_time(flight(Src, Dest, DepartTime), ArriveTime) :-
   compute_distance(Src, Dest, Distance),
   FlightTime is Distance / 500,
   hm_to_h(DepartTime, DepartTime_hours),
   ArriveTime is DepartTime_hours + FlightTime.

display_num_with_padding(Num) :-
   Num < 10, 
   write(0), 
   write(Num).

display_num_with_padding(Num) :-
   Num >= 10, 
   write(Num).

display_time(Time) :-
   X is floor(Time * 60),
   H is X // 60,
   M is X mod 60,
   display_num_with_padding(H), 
   write(':'), 
   display_num_with_padding(M).

capitalize([], []).
capitalize([H1|T1], [H2|T2]):-
    lower_upper(H1, H2),
    capitalize(T1, T2).

display_airport(Airport, AirportName) :-
    atom_chars(Airport, AirportChars),
    capitalize(AirportChars, UppAirportChars),
    format('~S', [UppAirportChars]),
    write('  '),
    write(AirportName),
    write('  ').

display_path([]).
display_path([flight(Depart, Arrive, StartTime)|Path]) :-
    airport(Depart, DepartName, _, _), airport(Arrive, ArriveName, _, _),
    write('depart  '), 
    display_airport(Depart, DepartName),
    hm_to_h(StartTime, DepartTime), display_time(DepartTime), nl,
    write('arrive  '), 
    display_airport(Arrive, ArriveName),
    compute_arrival_time(flight(Depart, Arrive, StartTime), ArriveTime),
    display_time(ArriveTime), nl, !,
    display_path(Path).
display_path(_).

valid_flight(DepartTime, ArriveTime, PrevArriveTime) :-
   hm_to_h(DepartTime, DepartTime_hours),
   (ArriveTime < 24),
   (DepartTime_hours > PrevArriveTime),
   ((DepartTime_hours - PrevArriveTime) > 0.5).

find_path(Src, Final, [flight(Src, Dest, DepartTime)|Path]) :-
   flight(Src, Dest, DepartTime),
   find_path(Dest, Final, [flight(Src, Dest, DepartTime)], Path).

find_path(Src, Src, _, [Src]).
find_path(Src, Final, [flight(PrevSrc, PrevDest, PrevDepartTime)|Tried], [flight(Src, Dest, DepartTime)|Path]) :-
   flight(Src, Dest, DepartTime),
   compute_arrival_time(flight(Src, Dest, DepartTime), ArriveTime),
   not(member(flight(Src, Dest, DepartTime), Tried)),
   compute_arrival_time(flight(PrevSrc, PrevDest, PrevDepartTime), PrevArriveTime),
   valid_flight(DepartTime, ArriveTime, PrevArriveTime),
   find_path(Dest, Final, [flight(Src, Dest, DepartTime)|Tried], Path).

fly(Src, Src) :-
    write('Source and desitination airports can not be equal'),
    nl, !, fail.

fly(Src, Dest) :-
   airport(Src, _, _, _),
   airport(Dest, _, _, _),
   find_path(Src, Dest, Path), nl,
   display_path(Path), !.

fly(Src, Dest) :-
   airport(Src, _, _, _),
   airport(Dest, _, _, _),
   write('No valid path exists'), nl, !, fail.

fly(Src, Dest) :-
    not(airport(Src, _, _, _)),
    airport(Dest, _, _, _),
    write('Source airport does not exist'), nl, !, fail.

fly(Src, Dest) :-
    airport(Src, _, _, _),
    not(airport(Dest, _, _, _)),
    write('Destination airport does not exist'), nl, !, fail.

fly(Src, Dest) :-
    not(airport(Src, _, _, _)),
    not(airport(Dest, _, _, _)),
    write('Source and destination airports do not exist'), nl, !, fail.
