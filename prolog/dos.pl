/*
7.2 Segundo Ejemplo: Familia
Dada la siguiente base de conocimientos:

Resolver los predicados hermano, tío, primo y abuelo. 
*/

progenitor(homero, bart).
progenitor(homero, lisa).
progenitor(homero, maggie).
progenitor(abe, homero).
progenitor(abe, jose).
progenitor(jose, pepe).
progenitor(mona, homero).
progenitor(jacqueline, marge).
progenitor(marge, bart).
progenitor(marge, lisa).
progenitor(marge, maggie).

hermanos(Hermano1, Hermano2) :- progenitor(Padre, Hermano1), 
progenitor(Padre, Hermano2), Hermano1 \= Hermano2.

tio(Tio, Sobrino) :- progenitor(Padre, Sobrino), hermanos(Padre, Tio).

primo(Primo, Persona) :- tio(Tio, Persona), progenitor(Tio, Primo).

abuelo(Abuelo, Nieto) :- progenitor(Padre, Nieto), progenitor(Abuelo, Padre).


