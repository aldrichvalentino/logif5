:- dynamic(at/2).
:- dynamic(i_am_at/1).

path(livingroom, e, lifestyleroom):- at(flashlight, in_bag).
path(livingroom, e, lifestyleroom):-
	write('Its too dark to go there, you cant see anything'), nl, fail.
path(livingroom, s, parentsroom).
path(livingroom, w, door):- at(red, in_bag),at(blue,in_bag),at(green,in_bag).
path(livingroom, w, door):-
	write('The Door is locked, it seems that you have to find the keys to unlock it'), nl, fail, !.

path(parentsroom, n, livingroom).
path(parentsroom, e, library):- at(key, in_bag).
path(parentsroom, e, library):-
	write('The Door is locked, it seems that you have to find a key to unlock it'), nl, fail.

path(library, w, parentsroom).

path(lifestyleroom, w, livingroom).
path(lifestyleroom, n, bathroom).
path(lifestyleroom, e, diningroom).

path(bathroom, s, lifestyleroom).

path(diningroom, w, lifestyleroom).
path(diningroom, u, kidsroom).
path(diningroom, e, kitchen).

path(kidsroom, d, diningroom).

path(kitchen, w, diningroom).
path(kitchen, e, backyard).
path(kitchen, d, warehouse).

path(backyard, w, kitchen).

path(warehouse, u, kitchen).
path(warehouse, e, maidsroom).

path(maidsroom, w, warehouse).

describe(livingroom):-
	write('You are in a living room.'),nl,
	write('To the east is a lifestyle room;'),nl,
	write('To the south is a parents room;'),nl,
	write('To the west is a door.'),nl.

describe(parentsroom):-
	write('You are in a parents room.'),nl,
	write('To the north is a living room.'), nl,
	write('To the east is a library.'),nl.

describe(library):-
	write('You are in a library.'),nl,
	write('To the west is a parents room.'),nl.

describe(lifestyleroom):-
	write('You are in a lifestyle room.'),nl,
	write('To the north is a bathroom;'),nl,
	write('To the east is a dining room;'),nl,
	write('To the west is a living room.'),nl.

describe(bathroom):-
	write('You are in a bathroom.'),nl,
	write('To the south is a lifestyle room.'),nl.

describe(diningroom):-
	write('You are in a dining room.'),nl,
	write('To the east is a kitchen;'),nl,
	write('To the west is a lifestyle room;'),nl,
	write('There is a stairs, you can go up from here.'),nl.

describe(kidsroom):-
	write('You are in a kidsroom.'),nl,
	write('There is a stairs, you can go down from here.'),nl.

describe(kitchen):-
	write('You are in a kitchen.'),nl,
	write('To the east is a backyard;'),nl,
	write('To the west is a dining room;'),nl,
	write('There is a stairs, you can go down from here.'),nl.

describe(backyard):-
	write('You are in a backyard.'),nl,
	write('To the east is a kitchen.'),nl.

describe(warehouse):-
	write('You are in a warehouse.'),nl,
	write('To the east is a maids room;'),nl,
	write('There is a stairs, you can go up from here.'),nl.

describe(maidsroom):-
	write('You are in a maids room.'),nl,
	write('To the west is a warehouse.'),nl.

at(object, livingroom).
at(key, livingroom).

i_am_at(livingroom).

notice(X) :-
	at(Y, X),
	write('There is a '), write(Y), write(' here.\n'),
	fail.

notice(_).

look :-
	i_am_at(X),
	describe(X),
	notice(X).

go(D) :-
	i_am_at(X),
	path(X, D, Y),
	retract(i_am_at(X)),
	asserta(i_am_at(Y)),
	look,
	!.

go(_) :-
	write('You can''t go that way.\n').

take(X) :-
	at(X, in_bag),
	write(X), write(' already in bag.\n'),
	!.

take(X) :-
	i_am_at(Y),
	at(X, Y),
	retract(at(X, Y)),
	asserta(at(X, in_bag)),
	write(X), write(' taken.\n'),
	!.

take(X) :-
	write(X), write(' not found here.\n').

n :- go(n).
e :- go(e).
s :- go(s).
w :- go(w).
u :- go(u).
d :- go(d).

save(F) :-
	tell(F),
	listing(i_am_at/1),
	listing(at/2),
	told.

start :-
	instructions,
	look,
	repeat,
	write('> '),
	read(X),
	call(X),
	fail.

instructions :-
        nl,
        write('Weclome to Hallowed!.'), nl,
        write('Available commands are:'), nl,
        write('start.                   -- to start the game.'), nl,
        write('n.  s.  e.  w.  u.  d.   -- to go in that direction.'), nl,
        write('take(Object).            -- to pick up an object.'), nl,
        write('drop(Object).            -- to put down an object.'), nl,
        write('look.                    -- to look around you again.'), nl,
        write('instructions.            -- to see this message again.'), nl,
        write('save(Filename).		 -- to save current game state.'), nl,
	write('load(Filename).          -- to load previously save state.'), nl,
	write('quit.			 -- to end the game.'), nl,
        nl.














