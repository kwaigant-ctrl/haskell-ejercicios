/*
7.1 Primer ejemplo: Gustos
Juan gusta de María. 
Pedro gusta de Ana y de Nora. 
Todos los que gustan de Nora gustan de Zulema. 
Julián gusta de las morochas y de las chicas con onda. 
*/

gusta(juan, maria).
gusta(pedro, ana).
gusta(pedro, nora).

% gusta(nora) => gusta(zulema)
gusta(Persona, zulema) :- gusta(Persona, nora).

morocha(juana).
tieneOnda(emilia).

gusta(julian, Alguien) :- morocha(Alguien).
gusta(julian, Alguien) :- tieneOnda(Alguien).


/*Mario gusta de las morochas con onda y de Luisa. 
Todos los que gustan de Ana y de Luisa, gustan de Laura. 
Después cambiar ese "y" por un "o". */

gusta(mario, luisa).
gusta(mario, Chica) :- morocha(Chica), tieneOnda(Chica).

gusta(juan, luisa).
gusta(juan, ana).
gusta(Chico, laura) :- gusta(Chico, ana), gusta(Chico, luisa).

gusta(Pibe, laura) :- gusta(Pibe, ana).
gusta(Pibe, laura) :- gusta(Pibe, luisa).
