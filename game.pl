/* Deklarasi variabel */

:- dynamic(at/3).
:- dynamic(i_am_at/1).

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
        write('examine(Object).         -- to look up the description of an object.'), nl,
        write('open(Object).            -- to open an object that can be open.'),nl,
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
  write('The smells is horrible here.'), nl,
  write('It seems that is something behind that bed!'),nl,
  write('LOOOKK!! A dead body!.').

describe(maidsroom):-
  write('You are in a maids room.'),nl,
  write('To the west is a warehouse.').

consciousnessbar(mrX) :-
    player(_,X,_), X is X - 5.

/* Pendefinisian Objek */
at(libraryKey, lifestyleroom, not).
at(television, lifestyleroom, fixed).
at(piano, lifestyleroom, fixed).
at(flashlight, parentsroom, not).
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
at(dollcastle, kidsroom, fixed).
at(strangebox, kidsroom, fixed).
at(bloodySaw, kidsroom, not).
at(shovel, storageroom, not).
at(hammer, storageroom, not).
at(gun, storageroom, not).
at(screwdriver, storageroom, not).
at(deadBody, maidsroom, not).
at(thirdPassKey, maidsroom,not).
at(bed, maidsroom, fixed).
at(lighter, maidsroom, not).

/* Deskripsi Objek */
examine(libraryKey) :- write('An old key to open the library room.\n').
examine(television) :- write('A televesion that has not been used for a while.\n').
examine(piano) :- write('A piano with classical compositions on top.\n').
examine(flashlight) :- write('A small flashlight.\n').
examine(familyPhoto) :- write('What a beautiful family photo... I wonder what happened to the family.\n').
examine(clock) :- write('The clock has stopped.\n').
examine(magicalBook) :- write('A really starnge book that can tell you wisdom and information about the house.\n').
examine(bookShelf) :- write('A bookshelf, there is a lot of old book on it, however there is one book that looks interesting..\n').
examine(fireplace) :- write('A nice fireplace in the middle of the library. The owner really knows how to live.\n').
examine(sink) :- write('A sink with running water. I wonder why the pipe is working...\n').
examine(bathtub) :- write('A bathtub covered with dirt.\n').
examine(shower) :- write('The clock has stopped.\n').
examine(table) :- write('A dusty old table that can be used to eat snacks.\n').
examine(drawer) :- write('.\n').
examine(refrigerator) :- write('A refrigerator, the electric source is unplugged.\n').
examine(stove) :- write('A stove, can be used to cook.\n').
examine(pan) :- write('Just an ordinary pan.\n').
examine(bloodyknife) :- write('A knife with a lot of blood on it, i wonder why....\n').
examine(spatula) :- write('An ordinary spatula, it is usually used to cook\n').
examine(strangelittleHill) :- write('A little hill, maybe it can be used to bury something.\n').
examine(fruitTrees) :- write('A Tree with a lot of fruits on it.\n').
examine(emptyChickenShack) :- write('Looks like it is a chicken shack, there is an egg inside.\n').
examine(strangeDoll) :- write('The clock has stopped.\n').
examine(dollcastle) :- write('The clock has stopped.\n').
examine(strangebox) :- write('The clock has stopped.\n').
examine(bloodysaw) :- write('A saw with a lot of blood on it, i wonder why.\n').
examine(shovel) :- write('Just an ordinary shovel.\n').
examine(hammer) :- write('Just an ordinary hammer.\n').
examine(gun) :- write('A gun, there is no bullet in it, maybe it has been used...\n').
examine(screwdriver) :- write('Just an ordinary screwdriver.\n').
examine(deadBody) :- write('A dead body that smells so bad, i wonder who this is...\n').
examine(thirdPassKey) :- write('.\n').
examine(bed) :- write('A bed that seems to be where someone usually sleep.\n').
examine(lighter) :- write('Just an ordinary lighter.\n').

/* Talk kepada NPC */
talk(magicalBook) :-
  i_am_at(library),
  write('Hello there stranger! You seem lost in confusion.. How can I assist you?\n').

talk(magicalBook) :-
  i_am_at(library),
  at(kidsRoomKey, in_bag, not),
  write('I see you have encountered with the strange uncontrollable doll upstairs'), nl,
  write('In order to calm the doll down, you have to read this spell'), nl,
  assertz(at(paperOfSpell, in_bag, not)),
  write('paperOfSpell taken'), nl,
  write('Good Luck!'), nl.


talk(strangeDoll) :-
  i_am_at(kidsroom),
  write('MOMMY?? I want to spend time with you..'), nl,
  write('Do not try to move me mommy, because I am in love with this room.').

/* Perintah Open */
listCanOpen([refrigerator,strangebox,emptyChickenShack]).

open(X) :-
  i_am_at(Y),
  at(X, Y, fixed),
  listCanOpen(L),
  \+ member(X,L),
  write('Object cannot be open!'), nl.

open(strangebox) :-
  i_am_at(Y),
  at(strangebox, Y, fixed),
  write('strangebox successfully opened..'), nl,
  \+ at(secondPassKey, in_bag, not), !,
  write('There is a secondPassKey'),
  assertz(at(secondPassKey, in_bag, not)).

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

/* perintah Drop */
drop(strangeDoll) :-
  at(strangeDoll, in_bag, not),
  i_am_at(_),
  retract(at(strangeDoll, in_bag, not)),
  assertz(at(strangeDoll, kidsroom, not)),
  write(strangeDoll), write(' strangely ran back into the kidsroom'),
  nl.

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

save_game(F) :-
  open(F, write, S),
  set_output(S),
  listing(at/3),
  listing(i_am_at/1),
  close(S),
  write('Save successful'),nl.

load_game(F) :-
  retractall(at(_,_,_)),
  retractall(i_am_at(_)),
  open(F, read, S),
  process_file(S),
  !,
  close(S),
  write('Load successful'),nl,
  look.

process_file(S) :-
  at_end_of_stream(S).

process_file(S) :-
  \+ at_end_of_stream(S),
  read(S, X),
  assertz(X),
  process_file(S).

do(n) :- n, !, fail.
do(e) :- e, !, fail.
do(s) :- s, !, fail.
do(w) :- w, !, fail.
do(u) :- u, !, fail.
do(d) :- d, !, fail.
do(save(F)) :- save_game(F), !, fail.
do(load(F)) :- load_game(F), !, fail.
do(stat) :- stat, !, fail.
do(drop(X)) :- drop(X), !, fail.
do(look) :- look, !, fail.
do(open(X)) :- open(X), !, fail.
do(examine(X)) :- examine(X), !, fail.
do(instructions) :- instructions, !, fail.
do(talk(X)) :- talk(X), !, fail.
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




