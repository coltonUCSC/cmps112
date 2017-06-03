not(X) :- X, !, fail.
not(_).

constants( List ) :-
   Pi is pi,
   E is e,
   Epsilon is epsilon,
   List = [Pi, E, Epsilon].

degree_to_radians(degmin(Degree, _), Radians) :-
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

distance(Airport1, Airport2, Distance) :-
   airport(Airport1, _, Lat1, Lon1),
   airport(Airport2, _, Lat2, Lon2),
   degree_to_radians(Lat1, Lat1_rad),
   degree_to_radians(Lon1, Lon1_rad),
   degree_to_radians(Lat2, Lat2_rad),
   degree_to_radians(Lon2, Lon2_rad),
   haversine_radians(Lat1_rad, Lon1_rad, Lat2_rad, Lon2_rad, Distance).

hm_to_h(time(H, M), HO) :-
    HO is H + M / 60.

twodigit(Digits) :-
   Digits < 10, print(0), print(Digits).
twodigit(Digits) :-
   Digits >= 10, print(Digits).

print_time(HO) :-
   MO is floor(HO * 60),
   H is MO // 60,
   M is MO mod 60,
   twodigit(H), print(':'), twodigit(M).

arrive_time(flight(Airport1, Airport2, time(H,M)), ArriveT) :-
    fly_time(Airport1, Airport2, FlightT),
    hm_to_h(time(H,M), DepartT),
    ArriveT is DepartT + FlightT.

fly_time(Airport1, Airport2, FlightT) :-
    distance(Airport1, Airport2, DistanceMiles),
    FlightT is DistanceMiles / 500.

writepath([]).
writepath([flight(Depart, Arrive, DTime)|Path]) :-
    airport(Depart, DName, _, _), airport(Arrive, AName, _, _),
    write('depart  '), write(Depart), write('  '), write(DName),
    write('  '), hm_to_h(DTime, DepartT), print_time(DepartT), nl,
    write('arrive  '), write(Arrive), write('  '), write(AName),
    write('  '), arrive_time(flight(Depart, Arrive, DTime), ArriveT),
    print_time(ArriveT), nl, !,
    writepath(Path).
writepath(_).

listpath(Node, End, [flight(Node, Next, DepartT)|Path]) :-
  flight(Node, Next, DepartT),
  write('Path: '),
  write(Path),
  nl,
  write(flight(Node,Next,DepartT)), nl,
  listpath(Next, End, [flight(Node, Next, DepartT)], Path).

listpath(Node, Node, _, [Node]).
listpath(Node, End, [flight(PrevD, PrevA, PrevT)|Tried], [flight(Node, Next, DepartT)|Path]) :-
   flight(Node, Next, DepartT),
   write('Path: '),
   write(Path),
   nl,
   write('Writing out path...'),
   nl,
   writepath(Path),
   nl,
   write(flight(Node,Next,DepartT)), 
   nl,
   arrive_time(flight(Node, Next, DepartT), ArriveT),
   not(member(flight(Node, Next, DepartT), Tried)),
   arrive_time(flight(PrevD, PrevA, PrevT), PrevArrival),
   hm_to_h(DepartT, CurrentDepart),
   Transfer is CurrentDepart - PrevArrival,
   write('Path: '),
   write(Path),
   nl,
   write('    TIME: '), write(PrevArrival), nl,
   write('    DEPART: '), write(CurrentDepart), nl,
   write('    TRANSFER: '), write(Transfer), nl,
   (CurrentDepart > PrevArrival),
   (ArriveT < 24),
   (Transfer > 0.5),
   write(Node), write('->'), write(Next), 
   write(' *arrive: '), write(ArriveT), nl, nl, 
   listpath(Next, End, [flight(Node, Next, DepartT)|Tried], Path).

fly(Depart, Arrive) :-
    airport(Depart, _, _, _), airport(Arrive, _, _, _),
    listpath(Depart, Arrive, Path), !,
    write('Path: '),
    write(Path),
    nl,
    writepath(Path), true.

fly(Depart, Depart) :- write('Zero-fly request.'), nl, !, fail.
fly(Depart, Arrive) :- 
    airport(Depart, _, _, _), airport(Arrive, _, _, _),
    write('Did not find valid path.'), nl, !, fail.
fly(_,_) :- write('Non-existent airport request.'), nl, !, fail.
