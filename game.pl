/* Deklarasi variabel */

:- dynamic(at/3).
:- dynamic(i_am_at/1).
:- dynamic(sanityBar/1).
:- dynamic(mrX_is_here/0).
:- dynamic(alive/1).
:- dynamic(win/0).
i_am_at(livingroom).
sanityBar(100).



instructions :-
        nl,
        write('Welcome to Hallowed!.'), nl,
        write('Available commands are:'), nl,
        write('start.                   -- to start the game.'), nl,
        write('stat.                    -- to see whats inside the bag.'), nl,
        write('n.  s.  e.  w.  u.  d.   -- to go in that direction.'), nl,
        write('take(Object).            -- to pick up an object.'), nl,
        write('drop(Object).            -- to put down an object.'), nl,
        write('look.                    -- to look around you again.'), nl,
        write('examine(Object).         -- to look up the description of an object.'), nl,
        write('open(Object).            -- to open an object that can be open.'),nl,
        write('use(Tools,Object).       -- to use a specific tool on an object.'),nl,
        write('instructions.            -- to see this message again.'), nl,
        write('save(Filename).          -- to save current game state, use apostrophe on filename.'), nl,
        write('load(Filename).          -- to load previously save state, use apostrophe on filename.'), nl,
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
path(diningroom, u, kidsroom):- at(kidsroomKey, in_bag, not).
path(diningroom, u, kidsroom):-
  write('The Door is locked, it seems that you have to find a key to unlock it'), nl, fail.
path(diningroom, e, kitchen).

path(kidsroom, d, diningroom).

path(kitchen, w, diningroom).
path(kitchen, e, backyard).
path(kitchen, d, storageroom).

path(backyard, w, kitchen).

path(storageroom, u, kitchen).
path(storageroom, e, maidsroom).

path(maidsroom, w, storageroom).

alive(mrX).
alive(magicalBook).
alive(strangeDoll).

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
  alive(magicalBook),
  write('*CRINK* *CRINK*'),nl,
  write('Library is the place for knowledge and information'), nl, 
  write('Just remember you are not alone. I am here to help you out.'), nl,
  write('The sound is comming from nowhere...'), nl.

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
  alive(strangeDoll),
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
  write('To the east is a backyard'),nl,
  write('To the west is a dining room'),nl,
  write('There is a stairs, you can go down from here.'),nl.

describe(backyard):-
  alive(mrX),
  write('This place is creeping me out...'),nl,
  write('Not a great idea to come here alone..'),nl,
  write('To the west is the kitchen.'),nl.

describe(backyard):-
  write('You are in a backyard.'),nl,
  write('To the east is a kitchen.').

describe(storageroom):-
  write('You are in a storageroom.\n'),
  write('To the east is a maids room;'),nl,
  write('There is a stairs, you can go up from here.'),nl.

describe(maidsroom):-
  at(deadBody, maidsroom, not),
  write('The smells is horrible here.'), nl,
  write('It seems that is something behind that bed!'),nl,
  write('LOOOKK!! A dead body!.'),nl.

describe(maidsroom):-
  write('You are in a maids room.'),nl,
  write('To the west is a storageroom.'),nl.
  
describe(door):-
  at(gun, in_bag, not),at(bloodyknife,in_bag, not),at(bloodySaw,in_bag, not),
  write('YOU WIN!!!'),
  write('You got out of the house, from the items you gathered you remembered about something.'),nl,
  write('A bloodyknife, a gun, a bloodysaw... Suddenly you realized that you killed all the members'),nl,
  write('of the family that lives in this house.'),nl,
  write('You quickly run away from the house and hide to avoid getting caught by the police.'),nl,
  assertz(win).
  
describe(door):- 
  write('YOU WIN!!!'),
  write('You got out of the house, but suddenly police surrounds you.'),nl,
  write('Police arrested you for killing all the members of the family.'),nl,
  write('Maybe you got an amnesia or something so you dont remember anything about this.'),nl,
  write('And the next day, you end up in jail for the crimes you commited.'),nl,
  write('Thank you for playing Hallowed, we are waiting here to haunt you next time :)\n'),
  assertz(win).
  
  
/* Pendefinisian Objek */
at(libraryKey, lifestyleroom, not).
at(television, lifestyleroom, fixed).
at(piano, lifestyleroom, fixed).
at(flashlight, parentsroom, not).
at(familyPhoto, parentsroom, fixed).
at(clock, parentsroom, fixed).
at(bookShelf, library, fixed).
at(fireplace, library, fixed).
at(sink, bathroom, fixed).
at(magicalBook, library, fixed).
at(bathtub, bathroom, fixed).
at(shower, bathroom, fixed).
at(table, diningroom, fixed).
at(cupboard, diningroom, fixed).
at(refrigerator, diningroom, fixed).
at(stove, kitchen, fixed).
at(pan, kitchen, not).
at(bloodyknife, kitchen, not).
at(spatula, kitchen, not).
at(strangeLittleHill, backyard, fixed).
at(fruitTree, backyard, fixed).
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
examine(shower) :- write('A shower, normally used to take a bath.\n').
examine(table) :- write('A dusty old table that can be used to eat snacks.\n').
examine(cupboard) :- write('A cupboard to store things.\n').
examine(refrigerator) :- write('A refrigerator, the electric source is unplugged.\n').
examine(stove) :- write('A stove, can be used to cook.\n').
examine(pan) :- write('Just an ordinary pan.\n').
examine(bloodyknife) :- write('A knife with a lot of blood on it, i wonder why....\n').
examine(spatula) :- write('An ordinary spatula, it is usually used to cook\n').
examine(strangeLittleHill) :- write('A little hill, maybe it can be used to bury something.\n').
examine(fruitTree) :- write('A Tree with a lot of fruits on it. --You can use open(fruitTree) here\n').
examine(emptyChickenShack) :- write('Looks like it is a chicken shack, there is an egg inside.\n').
examine(strangeDoll) :- write('A scary looking doll constantly saying "MOMMY MOMMY".\n').
examine(dollcastle) :- write('A small castle that appears to be a house for a toy.\n').
examine(strangebox) :- write('A strange box, mayber there is something in it.\n').
examine(bloodysaw) :- write('A saw with a lot of blood on it, i wonder why...\n').
examine(shovel) :- write('Just an ordinary shovel.\n').
examine(hammer) :- write('Just an ordinary hammer.\n').
examine(gun) :- write('A gun, there is no bullet in it, maybe it has been used...\n').
examine(screwdriver) :- write('Just an ordinary screwdriver.\n').
examine(deadBody) :- write('A dead body that smells so bad, i wonder who this is...\n').
examine(thirdPassKey) :- write('.\n').
examine(bed) :- write('A bed that seems to be where someone usually sleep.\n').
examine(lighter) :- write('Just an ordinary lighter.\n').
examine(hole):- write('A big hole as an outcome of your digging\n').
examine(egg):- write('An egg you find in the empty chicken shack\n').
examine(flour):- write('The flour that you find in the cupboard\n').
examine(apple):- write('The apple that you find in the fruitTree\n').

examine_check(X) :-
  i_am_at(Y),
  (at(X, Y, _); at(X, in_bag, _)),
  examine(X), 
  !.

examine_check(X) :-
  write(X), write(' cannot be examined.'), nl.


/* Talk kepada NPC */
talk(magicalBook) :-
  i_am_at(library),
  \+ at(kidsroomKey, in_bag, not),
  write('Hello there stranger! You seem lost in confusion.. How can I assist you?\n'),nl,
  write('I will tell everything that I know'),nl.
  
talk(magicalBook) :-
  i_am_at(library),
  at(kidsroomKey, in_bag, not),
  write('I see you have encountered with the strange uncontrollable doll upstairs.'), nl,
  write('In order to calm the doll down, you have to read this spell.'), nl,
  assertz(at(paperOfSpell, in_bag, not)),
  write('paperOfSpell taken.'), nl,
  write('Good Luck!'), nl.

talk(strangeDoll) :-
  i_am_at(kidsroom),
  alive(strangeDoll),
  write('MOMMY?? I want to spend time with you..'), nl,
  write('Do not try to move me mommy, because I am in love with this room.'), nl.

/* Perintah Open */
listCanOpen([refrigerator,strangebox,emptyChickenShack,cupboard,strangeLittleHill,fruitTree]).

open(refrigerator) :-
  i_am_at(P),
  at(refrigerator, P, fixed),
  write('EIIKK.. THERE IS A FROZEN HUMAN HEAD HERE!'), nl,
  retract(sanityBar(X)),
  Y is X - 10,
  assertz(sanityBar(Y)),
  write('You consciousness is now '),write(Y), nl.

open(strangeLittleHill) :-
  i_am_at(backyard),
  \+ at(shovel, in_bag, not),
  write('You need something to dig the hill.'),nl.

open(strangebox) :-
  i_am_at(kidsroom),
  alive(strangeDoll),
  write('DONT COME NEAR THAT BOX!'),nl.

open(strangebox) :-
  \+ alive(strangeDoll),
  i_am_at(kidsroom),
  write('You successfully opened the box!'),nl,
  assertz(at(secondPassKey, in_bag, not)),
  write('There is a secondPassKey!.'),nl,
  write('secondPassKey taken.'),nl.

open(emptyChickenShack) :-
  at(flour,in_bag,not),
  at(apple,in_bag,not),
  i_am_at(backyard),
  write('You see through the chicken shack.'),nl,
  write('There is an egg here! You take the egg.'),nl,
  write('Egg taken.'),nl,
  write('I think i have enough ingredients to make an apple pie!'),nl,
  assertz(at(egg, in_bag, not)).

open(emptyChickenShack) :-
  i_am_at(backyard),
  write('You see through the chicken shack.'),nl,
  write('There is an egg here! You take the egg.'),nl,
  write('Egg taken.'),nl,
  assertz(at(egg, in_bag, not)).
  
open(cupboard) :-
  i_am_at(diningroom),
  write('There is some flour here...'),nl,
  assertz(at(flour, in_bag, not)),
  write('flour taken.'), nl.

open(cupboard) :-
  at(egg, in_bag, not),
  at(apple, in_bag, not),
  i_am_at(diningroom),
  write('There is some flour here...'),nl,
  assertz(at(flour, in_bag, not)),
  write('flour taken.'), nl,
  write('I think i have enough ingredients to make an apple pie!'),nl.

open(fruitTree) :-
  i_am_at(backyard),
  write('Some apples on this tree has ripped. I can eat this.'),nl,
  assertz(at(apple, in_bag, not)),
  write('apple taken.'), nl.

open(fruitTree) :-
  at(egg, in_bag, not),
  at(flour, in_bag, not),
  i_am_at(backyard),
  write('Some apples on this tree has ripped. I can eat this.'),nl,
  assertz(at(apple, in_bag, not)),
  write('apple taken.'), nl,
  write('I think i have enough ingredients to make an apple pie!'),nl.

open(X):-
  i_am_at(Y),
  at(X, Y, fixed),
  listCanOpen(L),
  \+ member(X,L),
  write('Object cannot be opened!'), nl, !.

/* Perintah Use */
use(shovel,strangeLittleHill) :-
  i_am_at(backyard),
  at(shovel, in_bag, not),
  write('You dug the strange hill.'),nl,
  write('You found some dirty things.. You need to clean it.'),nl,
  assertz(at(dirtyRoomKey, in_bag, not)),
  assertz(at(dirtyPassKey, in_bag, not)),
  write('dirtyRoomKey and dirtyPassKey taken.'),nl,
  write('You left a big hole here'),nl,
  retract(at(strangeLittleHill, backyard, fixed)),
  assertz(at(hole, backyard, fixed)).
  
use(shovel,strangeLittleHill) :-
  i_am_at(backyard),
  \+ at(shovel, in_bag, not),
  write('You dont have the shovel.'),nl.
 
use(deadBody,hole) :-
  i_am_at(backyard),
  at(shovel, in_bag, not),
  at(deadBody, in_bag, not),
  write('You put the deadBody inside the hole and bury it.'),nl,
  write('Suddenly the sense  being followed went away..'),nl,
  retract(at(hole, backyard, fixed)),
  retract(at(deadBody, in_bag, not)),
  retract(alive(mrX)).

use(sink,dirtyRoomKey) :-
  i_am_at(bathroom),
  at(dirtyRoomKey, in_bag, not),
  write('With the running water, I can clean the key.'),nl,
  write('The key is clean now. Its the kidsroom key!'),nl,
  retract(at(dirtyRoomKey, in_bag, not)),
  assertz(at(kidsroomKey, in_bag, not)).

use(sink,dirtyPassKey) :-
  i_am_at(bathroom),
  at(dirtyPassKey, in_bag, not),
  write('With the running water, the key is cleaned'),nl,
  write('The key is clean now. Its the first pass key!'),nl,
  retract(at(dirtyPassKey, in_bag, not)),
  assertz(at(firstPassKey , in_bag, not)).

use(paperOfSpell,strangeDoll) :-
  write('BIDIBI BOBIDI BOOOO'),nl,
  write('Okay Mom! I think its time I go back to sleep'),nl,
  retract(alive(strangeDoll)).
  
use(lighter, stove) :-
  at(apple, in_bag, not),
  at(egg, in_bag, not),
  at(flour, in_bag, not),
  write('With some fruits, eggs, and flour that I found, I can make an apple pie.'),nl,
  write('It taste delicious.'),nl,
  retract(at(fruit, in_bag, not)),
  retract(at(egg, in_bag, not)),
  retract(at(flour, in_bag, not)).

use(lighter, stove) :-
  \+ at(apple, in_bag, not),
  \+ at(egg, in_bag, not),
  \+ at(flour, in_bag, not),
  write('I need some ingredients to cook'),nl.

use(X,_) :-
  write(X), write(' cannot be used.'),nl,!.



notice(X) :-
  at(Y, X, _),
  write('There is a '), write(Y), write(' here.\n'),
  fail.

notice(_).

look :-
  i_am_at(X),
  describe(X),
  notice(X).

sanityStat :-
  write('Current sanity level: '), sanityBar(X), write(X), write('%.'), nl.
  
appearMrX(0) :-
  alive(mrX),
  \+ win,
  write('OHH NO NO NO.'),nl,
  write('MR. X IS COMING!!'),nl,
  write('You better RUN NOW!!'),nl,
  retract(sanityBar(X)),
  Y is X - 5,
  assertz(sanityBar(Y)),
  sanityStat,
  assertz(mrX_is_here).
  
appearMrX(_) :- retractall(mrX_is_here).

go(D) :- 
  alive(mrX),
  i_am_at(X),
  path(X, D, Y),
  retract(i_am_at(X)),
  assertz(i_am_at(Y)),
  random(0, 9, R),
  appearMrX(R),
  look,
  !.

go(D) :-
  \+ alive(mrX),
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
  write(strangeDoll), write(' strangely ran back into the kidsroom.'),
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
  sanityStat,
  write('Bag contents: \n'),
  forall(at(X, in_bag, not), writeln(X)).

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
  listing(sanityBar/1),
  listing(mrX_is_here/0),
  close(S),
  write('Save successful'),nl.

load_game(F) :-
  file_exists(F),
  retractall(at(_,_,_)),
  retractall(i_am_at(_)),
  retractall(sanityBar(_)),
  retractall(mrX_is_here),  
  open(F, read, S),
  process_file(S),
  !,
  close(S),
  write('Load successful'),nl,
  look.
  
load_game(F) :-
  \+ file_exists(F),
  write('Load unsuccessful. File not found.'), nl.

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
do(examine(X)) :- examine_check(X), !, fail.
do(instructions) :- instructions, !, fail.
do(talk(X)) :- talk(X), !, fail.
do(take(X)) :- take(X), !, fail.
do(use(X,Y)) :- use(X,Y), !, fail.
do(quit) :- 
  write('Thank you for playing Hallowed, we are waiting here to haunt you next time :)\n').
do(_) :- write('Invalid command.\n'), !, fail.


game_loop :-
  repeat,
  ( write('> '), read(X), do(X) 
  ; win
  ; (sanityBar(X), X =< 0), 
  write('You died.\n\nGAME OVER\n\nThank you for playing Hallowed, we are waiting here to haunt you next time :)\n')).

start :-
  instructions,
  look,
  game_loop,
  !. 
