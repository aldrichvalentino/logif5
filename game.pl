/* Deklarasi variabel */

:- dynamic(at/3).
:- dynamic(i_am_at/1).
:- dynamic(player/3).

player([],50,50). /* list of item, consciousness bar, hunger bar */

i_am_at(livingroom).

instructions :-
        nl,
        write('Welcome to Hallowed!.'), nl,
        write('Available commands are:'), nl,
        write('start.                   -- to start the game.'), nl,
        write('n.  s.  e.  w.  u.  d.   -- to go in that direction.'), nl,
        write('take(Object).            -- to pick up an object.'), nl,
        write('drop(Object).            -- to put down an object.'), nl,
        write('look.                    -- to look around you again.'), nl,
        write('instructions.            -- to see this message again.'), nl,
        write('save(Filename).          -- to save current game state.'), nl,
        write('load(Filename).          -- to load previously save state.'), nl,
        write('quit.                    -- to end the game.'), nl,
        nl.

path(livingroom, e, lifestyleroom):- at(flashlight, in_bag, not).
path(livingroom, e, lifestyleroom):-
  write('Its too dark to go there, you cant see anything'), nl, fail.
path(livingroom, s, parentsroom).
path(livingroom, w, door):- at(firstPassKey, in_bag, not),at(secondPassKey,in_bag, not),at(thirdPassKey,in_bag, not).
path(livingroom, w, door):-
  write('The Door is locked, it seems that you have to find the keys to unlock it'), nl, fail, !.

path(parentsroom, n, livingroom).
path(parentsroom, e, library):- at(libraryKey, in_bag, not).
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

alive(mrX).
alive(doll).
alive(sorcerer).

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
  alive(sorcerer),
  write('Welcome pals, seems you stuck in here huh?'),nl,
  write('I am sorcerer. *CRINK* *CRINK*'),nl,
  write('Just remember you are not alone. I am here to help you out.'),nl.
  
  
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
  alive(doll),
  write('*Moommyy?? Lets playy*'),nl,
  write('Where is that sound coming from??'),nl,
  write('*Come on Mommy. I miss you*'),nl,
  write('THERE! That strange doll!'),nl,
  write('You need to find out how to make that doll calm down.'),nl.

describe(kidsroom):-
  write('You are in a kidsroom.'),nl,
  write('There is a stairs, you can go down from here.'),nl.

describe(kitchen):-
  write('You are in a kitchen.'),nl,
  write('To the east is a backyard;'),nl,
  write('To the west is a dining room;'),nl,
  write('There is a stairs, you can go down from here.'),nl.
  
describe(backyard):-
  alive(mrX),
  write('OHH NO NO NO.'),nl,
  write('Not a great idea you are came to the backyard.'),nl,
  write('This is place that people are often "meet" Mr.X!!'),nl,
  write('HE IS COMING!!'),nl,
  write('You better RUN NOW!!').

describe(backyard):-
  write('You are in a backyard.'),nl,
  write('To the east is a kitchen.').

describe(warehouse):-
  write('You are in a warehouse.'),nl,
  write('To the east is a maids room;'),nl,
  write('There is a stairs, you can go up from here.').

describe(maidsroom):-
  at(deadBody,maidsroom,not),
  write('The smells is horrible here.'), nl,
  write('It seems that is something behind that bed!'),nl,
  write('LOOOKK!! A dead body!.').
  
describe(maidsroom):-
  write('You are in a maids room.'),nl,
  write('To the west is a warehouse.').

consciousnessbar(mrX) :- 
    player(_,X,_), X is X - 5.

at(libraryKey, lifestyleroom, not).
at(television, lifestyleroom, fixed).
at(piano, lifestyleroom, fixed).
at(flashlight, parentsroom, not).
at(bed, parentsroom, fixed).
at(familyPhoto, parentsroom, fixed).
at(clock, parentsroom, fixed).
at(magicalBook, library, not).
at(bookShelf, library, fixed).
at(fireplace, library, fixed).
at(sink, bathroom, fixed).
at(bathtub, bathroom, fixed).
at(shower, bathroom, fixed).
at(table, diningroom, fixed).
at(drawer, diningroom, fixed).
at(refrigerator, diningroom, fixed).
at(stove, kitchen, fixed).
at(pan, kitchen, not).
at(bloodyknife, kitchen, not).
at(spatula, kitchen, not).
at(strangeLittleHill, backyard, fixed).
at(fruitTrees, backyard, fixed).
at(emptyChickenShack, backyard, fixed).
at(strangeDoll, kidsroom, not).
at(bed, kidsroom, fixed).
at(dollcastle, kidsroom, fixed).
at(strangebox, kidsroom, not).
at(bloodySaw, kidsroom, not).
at(shovel, storageroom, not).
at(hammer, storageroom, not).
at(gun, storageroom, not).
at(screwdriver, storageroom, not).
at(deadBody, maidsroom, not).
at(thirdPassKey, maidsroom,not).
at(bed, maidsroom, fixed).
at(lighter, maidsroom, not).

notice(X) :-
  at(Y, X, _),
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
  assertz(i_am_at(Y)),
  look,
  !.

go(_) :-
  write('You can''t go that way.\n').

take(X) :-
  at(X, in_bag, not),
  write(X), write(' already in bag.\n'),
  !.
  
take(X) :-
  i_am_at(Y),
  at(X, Y, fixed),
  write(X), write(' cannot be taken.\n'),
  !.

take(X) :-
  i_am_at(Y),
  at(X, Y, not),
  retract(at(X, Y, not)),
  assertz(at(X, in_bag, not)),
  write(X), write(' taken.\n'),
  !.

take(X) :-
  write(X), write(' not found here.\n').
  
  
drop(X) :-
        at(X, in_bag, not),
        i_am_at(Y),
        retract(at(X, in_bag, not)),
        assertz(at(X, Y, not)),
        write(X), write(' dropped.'),
        nl.

drop(X) :-
        write(X), write(' not in bag.'),
        nl.

writeln(X) :- write(X), nl.

stat :-
  write('Bag contents: \n'),
  forall(at(X, in_bag, not), writeln(X)).

/*
use(X).
talk(X).
load(X).
*/

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

do(n) :- n, !, fail.
do(e) :- e, !, fail.
do(s) :- s, !, fail.
do(w) :- w, !, fail.
do(u) :- u, !, fail.
do(d) :- d, !, fail.
do(stat) :- stat, !, fail.
do(drop(X)) :- drop(X), !, fail.
do(look) :- look, !, fail.
do(instructions) :- instructions, !, fail.
do(take(X)) :- take(X), !, fail.
do(quit).
do(_) :- write('Invalid command.\n'), !, fail.

game_loop :-
  repeat,
  write('> '),
  read(X),
  do(X).

start :-
  instructions,
  look,
  game_loop,
  !.