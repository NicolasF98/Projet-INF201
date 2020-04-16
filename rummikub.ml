(* FOURNOUT Nicolas INF-1 *)
(* Je ne suis pas en binome *)


(* Implémentation des notions de multielement *)

type nat = int (* >= 0 *)
type 'a multielement = 'a * nat (* 'a == reservoir des valeurs *)


(* Implémentation des notions de multiensemble sur type somme *)

type 'a multiensemble =
|V											(* V pour Vide *)
|A of 'a multielement * 'a multiensemble	(* A pour Ajout *)


(* Implémentation des fonctions de manipulation d'un multiensemble *)

let elt_of (elt, _ : 'a multielement) : 'a = elt;;		(* Donne la valeur de l'élément de notre multielement *)
let occ_of (_, occ : 'a multielement) : int = occ;;		(* Donne le nombre d'occurence de l'élément de notre multielement *)


(**
  | SPECIFICATION | cardinal
  | Profil: 'a multiensemble -> int
  | Sémantique: (cardinal ens) est le nombre total d'éléments de ens.
  | Examples:
  | 	cardinal (A((3,2), A((4,1), A((6,3), V)))) = 6
  |
  | REALISATION |
  | Algorithme: Utilisation de la recurtion et du pattern matching.
  | Equations:
  | 	(1) cardinal V = 0
  |   	(2) cardinal A ((valu, nbr), ensprime) = nbr + cardinal ensprime
  | Implémentation:
**)

let rec cardinal (ens:'a multiensemble) : int =
  match ens with
  | V -> 0
  | A ((_, nbr), ensprime) -> nbr + (cardinal ensprime)
;;

let _ = assert(cardinal (A((3,2), A((4,1), A((6,3), V)))) = 6);;


(**
  | SPECIFICATION | nombre d'occurrences
  | Profil: 'a -> 'a multiensemble -> int
  | Sémantique: (nbocc elt ens) nombre d'occurrence de elt dans ens.
  | Examples:
  | 	nbocc 6 (A((3,2), A((4,1), A((6,3), V)))) = 3
  |
  | REALISATION |
  | Algorithme: Utilisation de la recurtion, du pattern matching, et de if...then...else.
  | Equations:
  | 	(1) nbocc V = 0
  |   	(2) nbocc A ((valu, nbr), ensprime) = if (e = elt) then nbr else (nbocc elt ensprime)
  | Implémentation:
**)

let rec nbocc (elt: 'a) (ens:'a multiensemble) : int =
  match ens with
  | V -> 0
  | A ((e, nbr), ensprime) -> if (e = elt) then nbr else (nbocc elt ensprime)
;;

let _ = assert(nbocc 6 (A((3,2), A((4,1), A((6,3), V)))) = 3);;


(**
  | SPECIFICATION | appartenance
  | Profil: 'a -> 'a multiensemble -> bool
  | Sémantique: (appartient elt ens) renvoie True si elt est dans ens, sinon False.
  | Examples:
  | 	appartient 6 (A((3,2), A((4,1), A((6,3), V)))) = True
  |		appartient 9 (A((3,2), A((4,1), A((6,3), V)))) = False
  |
  | REALISATION |
  | Algorithme: Utilisation de la recurtion, du pattern matching, et de connecteur logique << ou >> ||.
  | Equations:
  | 	(1) appartient V = False
  |   	(2) appartient A ((valu, nbr), ensprime) = (elt = e) || (nbocc elt ensprime)
  | Implémentation:
**)

let rec appartient (elt: 'a) (ens:'a multiensemble) : bool =
  match ens with
  | V -> false
  | A ((e, nbr), ensprime) -> (e = elt) || (appartient elt ensprime)
;;

let _ = assert(appartient 6 (A((3,2), A((4,1), A((6,3), V)))) = true)
and _ = assert(appartient 9 (A((3,2), A((4,1), A((6,3), V)))) = false);;


(**
  | SPECIFICATION | inclusion
  | Profil: 'a multiensemble -> 'a multiensemble -> bool
  | Sémantique: (inclus ens1 ens2) renvoie True si ens1 est dans ens2, sinon False.
  | Examples:
  | 	inclus (A((3,2), V)) (A((3,2), A((4,1), A((6,3), V)))) = True
  |		inclus (A((9,1), V)) (A((3,2), A((4,1), A((6,3), V)))) = False
  |
  | REALISATION |
  | Algorithme: Utilisation de la recurtion, du pattern matching, et de connecteur logique << et >> &&.
  | Equations:
  | 	(1) V  -> true  (* Si ens1 est vide, alors il est inclus dans ens2 *)
  |   	(2) A ((e, nbr), ensprime) -> (appartient e ens2) && (inclus ensprime ens2)
  | Implémentation:
**)

let rec inclus (ens1:'a multiensemble) (ens2:'a multiensemble) : bool =
  match ens1 with
  | V -> true 
  | A ((e, nbr), ensprime) -> (appartient e ens2) && ((nbocc e ens2) = nbr) && (inclus ensprime ens2)
;;

let _ = assert(inclus (A((3,2), V)) (A((3,2), A((4,1), A((6,3), V)))) = true)
and _ = assert(inclus (A((9,1), V)) (A((3,2), A((4,1), A((6,3), V)))) = false);;


(**
  | SPECIFICATION | ajout
  | Profil: 'a multielement -> 'a multiensemble -> 'a multiensemble
  | Sémantique: (ajout elt ens) ajoute elt dans ens, en prenant en compte le principe de non-répétition du multi-ensemble.
  | Examples:
  | 	ajout (1,2) (A((3,2), A((4,1), A((6,3), V)))) = (A((1,2),A((3,2), A((4,1), A((6,3), V))))) 
  | REALISATION |
  | Algorithme: Utilisation du conditionnelle If...Then...Else.
  | Equations:
  | 	(1) Si l'elt est dans ens, alors on renvoit ens, sinon on l'ajoute
  | Implémentation:
**)

let ajout (elt: 'a multielement) (ens: 'a multiensemble) : 'a multiensemble =
	if (appartient (elt_of elt) ens) then ens else A(((elt_of elt),(occ_of elt)),ens)
;;

let _ = assert(ajout (1,2) (A((3,2), A((4,1), A((6,3), V)))) = (A((1,2),A((3,2), A((4,1), A((6,3), V))))));;


(**
  | SPECIFICATION | supprime
  | Profil: 'a multielement -> 'a multiensemble -> 'a multiensemble
  | Sémantique: (supprime elt ens) supprime n occurence de elt de ens, si occ_of elt est égale à 0 et/ou elt_of elt n'est pas dans ens, alors on renvoie ens.
  | Examples:
  | 	supprime (3,2) (A((3,2), A((4,1), A((6,3), V)))) = A((4,1), A((6,3), V)) 
  |		supprime (3,0) (A((3,2), A((4,1), A((6,3), V)))) = A((3,2), A((4,1), A((6,3), V)))
  | REALISATION |
  | Algorithme: Utilisation du conditionnelle If...Then..Else, de la recurtion, et du pattern matching.
  | Equations:
  | 	(1) Si l'elt est dans ens, alors on renvoit ens, sinon on l'ajoute
  | Implémentation:
**)

let rec supprime (elt: 'a multielement) (ens: 'a multiensemble) : 'a multiensemble =
	match ens with
	| V -> V
	| A ((e, o), ens) -> if (e = (elt_of elt)) then A((e, o - (occ_of elt)), ens) else A ((e, o), (supprime elt ens))
;;

let _ = assert(supprime (3,2) (A((3,2), A((4,1), A((6,3), V)))) = A((3,0), A((4,1), A((6,3), V))))
and _ = assert(supprime (3,0) (A((3,2), A((4,1), A((6,3), V)))) = A((3,2), A((4,1), A((6,3), V))));;


(**
  | SPECIFICATION | egalité
  | Profil: 'a multiensemble -> 'a multiensemble -> bool
  | Sémantique: (egaux ens1 ens2) renvoie true si les deux ens ont les memes multielement, sinon false.
  | Examples:
  | 	egaux (A((3,2), A((4,1), A((6,3), V)))) (A((3,2), A((4,1), A((6,3), V)))) = true 
  | 	egaux (A((3,7), A((4,1), A((6,3), V)))) (A((3,2), A((4,1), A((6,3), V)))) = false
  | REALISATION |
  | Algorithme: Utilisation du conditionnelle If...Then..Else, utilisation de la double inclusion.
  | Equations:
  | 	(1) Si ens1 est dans ens2, et si ens2 est dans ens1, alors true, sinon false.
  | Implémentation:
**)

let rec egaux (ens1: 'a multiensemble) (ens2: 'a multiensemble) : bool =
	if (inclus ens1 ens2) && (inclus ens2 ens1) then true else false
;;

let _ = assert(egaux (A((3,2), A((4,1), A((6,3), V)))) (A((3,2), A((4,1), A((6,3), V)))) = true)
and _ = assert(egaux (A((3,7), A((4,1), A((6,3), V)))) (A((3,2), A((4,1), A((6,3), V)))) = false);;


(**
  | SPECIFICATION | intersection
  | Profil: 'a multiensemble -> 'a multiensemble -> 'a multiensemble
  | Sémantique: (intersection ens1 ens2) est le multiensemble des multielement qui appartient à la fois à ens1 et ens2.
  | Examples:
  |   (a) intersection A((1,2), A((2,2), A((3,2), V))) A((4,2), A((2,2), A((3,2), V))) = A((2,2), A((3,2), V))
  | REALISATION |
  | Implémentation:
**)

let rec intersection (ens1:'a multiensemble) (ens2:'a multiensemble) : 'a multiensemble =
  match ens1 with
  | V -> V
  | A ((e,occ), ens) -> if (appartient e ens2) then A((e,occ), intersection ens ens2) else intersection ens ens2
;;

let _ = assert(intersection (A((1,2), A((2,2), A((3,2), V)))) (A((4,2), A((2,2), A((3,2), V)))) = A((2,2), A((3,2), V)));;


(**
  | SPECIFICATION | difference
  | Profil: 'a multiensemble -> 'a multiensemble -> 'a multiensemble
  | Sémantique: (difference ens1 ens2) est le multiensemble des multielement qui appartient à ens1, mais pas à ens2.
  | Examples:
  |   (a) difference A((1,2), A((2,2), A((3,2), V))) A((4,2), A((2,2), A((3,2), V))) = A((1,2), V)
  | REALISATION |
  | Implémentation:
**)

let rec difference (ens1:'a multiensemble) (ens2:'a multiensemble) : 'a multiensemble =
  match ens1 with
  | V -> V
  | A ((e,occ), ens) -> if (appartient e ens2) == false then A((e,occ), difference ens ens2) else difference ens ens2
;;

let _ = assert(difference (A((1,2), A((2,2), A((3,2), V)))) (A((4,2), A((2,2), A((3,2), V)))) = A((1,2),V));;


(**
  | SPECIFICATION | iem
  | Profil: 'a multiensemble -> int -> 'a
  | Sémantique: (iem ens i) renvoi un l'élément i de ens.
  | Examples:
  |   (a) (iem (A((1,2), A((13,2), A((30,2), V)))) 2) = 1 
  | REALISATION |
  | Implémentation:
**)

let rec iem (ens: 'a multiensemble) (i: int): 'a =
	match ens with
	| V -> 0
	| A((e,occ),ensprime) -> if (occ >= i) then e else (iem ensprime (i-occ));;

let _ = assert((iem (A((3,2), A((4,1), A((6,3), V)))) 2) = 3)
and _ = assert((iem (A((3,2), A((4,1), A((6,3), V)))) 3) = 4);;


(**
  | SPECIFICATION | un_dans
  | Profil: 'a multiensemble -> 'a
  | Sémantique: (und_dans ens) renvoi un élément aléatoire de ens, en prenant en compte la repetition (plus un elt et présent, plus il a de chance d'etre renvoyé).
  | Examples:
  |   (a) un_dans A((1,2), A((2,2), A((3,2), V))) A((4,2), A((2,2), A((3,2), V))))
  | REALISATION |
  | Implémentation:
**)

let un_dans(ens: 'a multiensemble): 'a=
	((iem ens (Random.int((cardinal ens)+1))));;

let _ = (un_dans(A((3,2), A((4,1), A((6,3), V)))));;



(* PARTIE Q2: REUSINAGE DU CODE - UTILISATION DU TYPE LISTE *)

(* PS: Pour différencer les fonctions reusiné, le suffixe L serra ajouté à chaque nom de fonction *)


(* Redefinition du type multiensemble *)

type 'a multiensemble = 'a multielement list;;


(**
  | SPECIFICATION | cardinalL
  | Profil: 'a multielement list -> int
  | Sémantique: (cardinalL ens) est le nombre d'elements de ens.
  | Examples:
  |   (a) cardinalL [ (2, 5) ;(3, 1) ] = 6
  |   (b) cardinalL [ (10, 3) ] = 3
  |   (c) cardinalL [ (true, 2); (false, 2) ] = 4
*)

let rec cardinalL (ens:'a multiensemble) : int=
  match ens with
  | [] -> 0
  | (valu,occ)::fin -> (cardinalL fin) + occ 
;;

let _ = assert(cardinalL [ (2, 5); (1,2) ] = 7 )
and _ = assert(cardinalL [ (102, 1) ] = 1)
and _ = assert(cardinalL [ (true,1) ; (false,2) ] = 3);;


(**
  | SPECIFICATION | nombre d'occurrences
  | Profil: 'a -> 'a multiensemble -> int
  | Sémantique: (nboccL elt ens) nombre d'occurrence de elt dans ens.
  | Examples:
  | 	(a) nboccL 6 [ (2,1); (6,3) ] = 3
**)

let rec nboccL (elt: 'a) (ens:'a multiensemble) : int =
  match ens with
  | [] -> 0
  | (valu,occ)::fin -> if (valu = elt) then occ else (nboccL elt fin)
;;

let _ = assert(nboccL 2 [(2,1); (3,2); (10,1)] = 1)
and _ = assert(nboccL 4 [(2,1); (3,1); (4,0)] = 0);;


(**
  | SPECIFICATION | appartientL
  | Profil: 'a -> 'a multiensemble -> bool
  | Sémantique: (appartientL elt ens) est vrai si et seulement si elt appartient à ens
  | Examples:
  |   (a) appartientL 2 [(2,1); (3,2); (10,1)] = true
  |   (b) appartientL 4 [(2,1); (3,1); (4,0)] = false
*)

let rec appartientL (elt:'a) (ens:'a multiensemble) : bool =
  match ens with
  | [] -> false
  | (valu,occ)::fin -> ((valu == elt) && (occ != 0)) || (appartientL elt fin)
;;

let _ = assert(appartientL 2 [(2,1); (3,2); (10,1)] = true)
and _ = assert(appartientL 4 [(2,1); (3,1); (4,0)] = false);;


(**
  | SPECIFICATION | inclusL
  | Profil: 'a multiensemble -> 'a multiensemble -> bool
  | Sémantique: (inclusL ens1 ens2) est vrai si et seulement si ens1 est inclus dans ens2
  | Examples:
  |   (a) inclusL [(1,2); (2,3); (3,3)] [(0,1); (1,2); (2,3); (3,3)] = true
  |   (b) inclusL [(0,1)] [(1,2); (2,3); (3,3)] = false
 *)

let rec inclusL (ens1:'a multiensemble) (ens2:'a multiensemble) : bool=
  match ens1 with
  | [] -> true (* eq. 1 *)
  | (valu,occ)::fin -> (appartientL valu ens2) && (inclusL fin ens2)
;;

let _ = assert(inclusL [(1,2); (2,3); (3,3)] [(0,1); (1,2); (2,3); (3,3)] = true)
and _ = assert(inclusL [(0,1)] [(1,2); (2,3); (3,3)] = false);;


(**
  | SPECIFICATION | ajouteL
  | Profil: 'a multielement -> 'a multiensemble -> 'a multiensemble
  | Sémantique: (ajouteL elt ens) est l'ensemble obtenu en ajoutant l'élément elt à l'ensemble ens en respectant la contrainte 
                de non répétition des éléments.
  | Examples:
  |   (a) ajouteL (7,1) [(0,1); (1,1); (2,1); (3,1); (4,1); (5,1)] = [(0,1); (1,1); (2,1); (3,1); (4,1); (5,1); (7,1)] 
  |   (b) ajouteL (1,1) [(0,1); (1,1); (2,1); (3,1); (4,1); (5,1)] =  [(0,1); (1,1); (2,1); (3,1); (4,1); (5,1)]
*)

let ajouteL (elt:'a multielement) (ens: 'a multiensemble): 'a multiensemble =
  if (appartientL (elt_of elt) ens) then ens
  else ens @ [(elt_of elt), (occ_of elt)]
;;

let _ = assert(ajouteL (7,1) [(0,1); (1,1); (2,1); (3,1); (4,1); (5,1)] = [(0,1); (1,1); (2,1); (3,1); (4,1); (5,1); (7,1)])
and _ = assert(ajouteL (1,1) [(0,1); (1,1); (2,1); (3,1); (4,1); (5,1)] =  [(0,1); (1,1); (2,1); (3,1); (4,1); (5,1)]);;


(**
  | SPECIFICATION | supprimeL
  | Profil: 'a multiensemble -> 'a multiensemble -> 'a multiensemble
  | Sémantique: (supprimeL elt ens) supprime l'élément elt de l'ensemble ens.
  | Examples:
  |   (a) supprimeL (3,1) [(1,1); (2,2); (3,1)] = [(1,1); (2,2)]
  |   (c) supprimeL (2,1) [(1,1); (2,2); (3,1)] = [(1,1); (2,1); (3,1)]
*)

let rec supprimeL (elt:'a multielement) (ens: 'a multiensemble): 'a multiensemble =
  match ens with
  | [] -> []
  | (valu, occ)::fin ->
      if (valu = (elt_of elt)) then (valu,occ - occ_of elt)::fin
      else (valu,occ)::(supprimeL elt fin)
;;

let _ = assert(supprimeL ('a',1) [('a',1); ('b',1)] = [('a',0);('b',1)])
and _ = assert(supprimeL (2,1) [(1,1); (2,2); (3,1)] = [(1,1); (2,1); (3,1)]);;


(**
  | SPECIFICATION | egauxL
  | Profil: 'a mutliensemble -> 'a multiensemble -> bool
  | Sémantique: (egauxL ens1 ens2) est vrai si et seulement si ens1 et ens2 ont les mêmes éléments.
  | Examples:
  |   (a) egauxL [(1,1); (2,1)] [(2,1); (1,1)] = True
  |   (b) egauxL [] [] = True
  |   (c) egauxL [(2,2); (3,1)] [(2,2); (3,1); (4,1)] = False
*)

let egauxL (ens1:'a multiensemble) (ens2: 'a multiensemble) : bool =
  (inclusL ens1 ens2) && (inclusL ens2 ens1);;

let _ = assert(egauxL [(1,1); (2,1)] [(2,1); (1,1)] = true)
and _ = assert(egauxL [] [] = true)
and _ = assert(egauxL [(2,2); (3,1)] [(2,2); (3,1); (4,1)] = false);;


(**
  | SPECIFICATION | intersectionL
  | Profil: 'a multiensemble -> 'a multiensemble -> 'a multiensemble
  | Sémantique: (intersectionL ens1 ens2) est l'ensemble qui appartient à la fois à ens1 et ens2.
  | Examples:
  |   (a) intersectionL [(1,1); (2,1); (3,1)] [(2,1); (3,1); (4,1)] = [(2,1); (3,1)]
  |   (c) intersectionL [(1,1); (2,1)] [(2,1); (1,1)] = [(1,1); (2,1)]
**)

let rec intersectionL (ens1:'a multiensemble) (ens2:'a multiensemble) : 'a multiensemble =
  match ens1 with
  | [] -> [] 
  | (valu,occ)::fin ->
      if (appartientL valu ens2) then (valu,occ)::(intersectionL fin ens2)
      else intersectionL fin ens2
;;

let _ = assert(intersectionL [(1,1); (2,1); (3,1)] [(2,1); (3,1); (4,1)] = [(2,1); (3,1)])
and _ = assert(intersectionL [(1,1); (2,1)] [(2,1); (1,1)] = [(1,1); (2,1)]);;


(**
  | SPECIFICATION | differenceL
  | Profil: 'a multiensemble -> 'a multiensemble -> 'a multiensemble
  | Sémantique: (differenceL ens1 ens2) est l'ensemble des elements qui appartiennent à ens1 mais pas à ens2
  | Examples:
  |   (a) differenceL [(2,1); (1,1)] [(1,1)] = [(2,1); (1,0)]
**)

let rec differenceL (ens1:'a multiensemble) (ens2:'a multiensemble) : 'a multiensemble =
  match ens2 with
  | [] -> ens1
  | (valu,occ)::fin -> differenceL (supprimeL (valu,occ) ens1) fin
;;

let _ = assert(differenceL [(2,1); (1,1)] [(1,1)] = [(2,1); (1,0)])


(**
  | SPECIFICATION | iemL
  | Profil: 'a multiensemble -> int -> 'a
  | Sémantique: (iem ens i) renvoi un l'élément i de ens.
  | Examples:
  |   (a) (iem (A((1,2), A((13,2), A((30,2), V)))) 2) = 1 
  | REALISATION |
  | Implémentation:
**)

let rec iemL (ens: 'a multiensemble) (i: int): 'a =
	match ens with
	| [] -> 0
	| (e,occ)::ensprime -> if (occ >= i) then e else (iemL ensprime (i-occ));;

let _ = assert((iemL [(3,2); (4,1); (6,3)] 2) = 3)
and _ = assert((iemL [(3,2); (4,1); (6,3)] 3) = 4);;


(**
  | SPECIFICATION | un_dansL
  | Profil: 'a multiensemble -> 'a
  | Sémantique: (und_dans ens) renvoi un élément aléatoire de ens, en prenant en compte la repetition (plus un elt et présent, plus il a de chance d'etre renvoyé).
  | Examples:
  |   (a) un_dansL [(3,2); (4,1); (6,3)]  
  | REALISATION |
  | Implémentation:
**)

let un_dansL(ens: 'a multiensemble): 'a=
	((iemL ens (Random.int((cardinalL ens)+1))));;

let _ = (un_dansL [(3,2); (4,1); (6,3)]);;




(* Q3 : Réusinage des fonctions avec l'ordre supérieur *)

(* PS: Pour différencer les fonctions reusiné, le suffixe 0 serra ajouté à chaque nom de fonction *)


(* Redefinition du type multiensemble *)

type 'a multiensemble = 'a multielement list;;


(**
  | SPECIFICATION | cardinalO
  | Profil: 'a multielement list -> int
  | Sémantique: (cardinalL ens) est le nombre d'elements de ens.
  | Examples:
  |   (a) cardinalO [ (2, 5) ;(3, 1) ] = 6
  |   (b) cardinal0 [ (10, 3) ] = 3
  |   (c) cardinal0 [ (true, 2); (false, 2) ] = 4
*)

(*let rec cardinalO (ens:'a multiensemble) : int=
	match ens with
	| [] -> 0
	| (valu,occ)::fin -> cardinalO fin + List.rev(List.hd(valu,occ))
;;

let _ = assert(cardinalO [ (2, 5); (1,2) ] = 7 )
and _ = assert(cardinalO [ (102, 1) ] = 1)
and _ = assert(cardinalO [ (true,1) ; (false,2) ] = 3);;
*)

(**
  | SPECIFICATION | nombre d'occurrences
  | Profil: 'a -> 'a multiensemble -> int
  | Sémantique: (nboccL elt ens) nombre d'occurrence de elt dans ens.
  | Examples:
  | 	(a) nboccL 6 [ (2,1); (6,3) ] = 3
**)

(*let nboccO (elt: 'a) (ens:'a multiensemble) : int =
	List.fold_left  elt ens
;;

let _ = assert(nboccO 2 [(2,1); (3,2); (10,1)] = 1)
and _ = assert(nboccL 4 [(2,1); (3,1); (4,0)] = 0);;

*)
(**
  | SPECIFICATION | appartientL
  | Profil: 'a -> 'a multiensemble -> bool
  | Sémantique: (appartientL elt ens) est vrai si et seulement si elt appartient à ens
  | Examples:
  |   (a) appartientL 2 [(2,1); (3,2); (10,1)] = true
  |   (b) appartientL 4 [(2,1); (3,1); (4,0)] = false
*)

(*let rec appartientL (elt:'a) (ens:'a multiensemble) : bool =
  match ens with
  | [] -> false
  | (valu,occ)::fin -> ((valu == elt) && (occ != 0)) || (appartientL elt fin)
;;

let _ = assert(appartientL 2 [(2,1); (3,2); (10,1)] = true)
and _ = assert(appartientL 4 [(2,1); (3,1); (4,0)] = false);;
*)

(**
  | SPECIFICATION | inclusL
  | Profil: 'a multiensemble -> 'a multiensemble -> bool
  | Sémantique: (inclusL ens1 ens2) est vrai si et seulement si ens1 est inclus dans ens2
  | Examples:
  |   (a) inclusL [(1,2); (2,3); (3,3)] [(0,1); (1,2); (2,3); (3,3)] = true
  |   (b) inclusL [(0,1)] [(1,2); (2,3); (3,3)] = false
 *)
(*
let rec inclusL (ens1:'a multiensemble) (ens2:'a multiensemble) : bool=
  match ens1 with
  | [] -> true (* eq. 1 *)
  | (valu,occ)::fin -> (appartientL valu ens2) && (inclusL fin ens2)
;;

let _ = assert(inclusL [(1,2); (2,3); (3,3)] [(0,1); (1,2); (2,3); (3,3)] = true)
and _ = assert(inclusL [(0,1)] [(1,2); (2,3); (3,3)] = false);;
*)

(**
  | SPECIFICATION | ajouteL
  | Profil: 'a multielement -> 'a multiensemble -> 'a multiensemble
  | Sémantique: (ajouteL elt ens) est l'ensemble obtenu en ajoutant l'élément elt à l'ensemble ens en respectant la contrainte 
                de non répétition des éléments.
  | Examples:
  |   (a) ajouteL (7,1) [(0,1); (1,1); (2,1); (3,1); (4,1); (5,1)] = [(0,1); (1,1); (2,1); (3,1); (4,1); (5,1); (7,1)] 
  |   (b) ajouteL (1,1) [(0,1); (1,1); (2,1); (3,1); (4,1); (5,1)] =  [(0,1); (1,1); (2,1); (3,1); (4,1); (5,1)]
*)

let ajouteO (elt:'a multielement) (ens: 'a multiensemble): 'a multiensemble =
	List.append ens elt
;;

let _ = assert(ajouteO (7,1) [(0,1); (1,1); (2,1); (3,1); (4,1); (5,1)] = [(0,1); (1,1); (2,1); (3,1); (4,1); (5,1); (7,1)])
and _ = assert(ajouteO (1,1) [(0,1); (1,1); (2,1); (3,1); (4,1); (5,1)] =  [(0,1); (1,1); (2,1); (3,1); (4,1); (5,1)]);;


(**
  | SPECIFICATION | supprimeL
  | Profil: 'a multiensemble -> 'a multiensemble -> 'a multiensemble
  | Sémantique: (supprimeL elt ens) supprime l'élément elt de l'ensemble ens.
  | Examples:
  |   (a) supprimeL (3,1) [(1,1); (2,2); (3,1)] = [(1,1); (2,2)]
  |   (c) supprimeL (2,1) [(1,1); (2,2); (3,1)] = [(1,1); (2,1); (3,1)]
*)

let rec supprimeL (elt:'a multielement) (ens: 'a multiensemble): 'a multiensemble =
  match ens with
  | [] -> []
  | (valu, occ)::fin ->
      if (valu = (elt_of elt)) then (valu,occ - occ_of elt)::fin
      else (valu,occ)::(supprimeL elt fin)
;;

let _ = assert(supprimeL ('a',1) [('a',1); ('b',1)] = [('a',0);('b',1)])
and _ = assert(supprimeL (2,1) [(1,1); (2,2); (3,1)] = [(1,1); (2,1); (3,1)]);;


(**
  | SPECIFICATION | egauxL
  | Profil: 'a mutliensemble -> 'a multiensemble -> bool
  | Sémantique: (egauxL ens1 ens2) est vrai si et seulement si ens1 et ens2 ont les mêmes éléments.
  | Examples:
  |   (a) egauxL [(1,1); (2,1)] [(2,1); (1,1)] = True
  |   (b) egauxL [] [] = True
  |   (c) egauxL [(2,2); (3,1)] [(2,2); (3,1); (4,1)] = False
*)

let egauxL (ens1:'a multiensemble) (ens2: 'a multiensemble) : bool =
  (inclusL ens1 ens2) && (inclusL ens2 ens1);;

let _ = assert(egauxL [(1,1); (2,1)] [(2,1); (1,1)] = true)
and _ = assert(egauxL [] [] = true)
and _ = assert(egauxL [(2,2); (3,1)] [(2,2); (3,1); (4,1)] = false);;


(**
  | SPECIFICATION | intersectionL
  | Profil: 'a multiensemble -> 'a multiensemble -> 'a multiensemble
  | Sémantique: (intersectionL ens1 ens2) est l'ensemble qui appartient à la fois à ens1 et ens2.
  | Examples:
  |   (a) intersectionL [(1,1); (2,1); (3,1)] [(2,1); (3,1); (4,1)] = [(2,1); (3,1)]
  |   (c) intersectionL [(1,1); (2,1)] [(2,1); (1,1)] = [(1,1); (2,1)]
**)

let rec intersectionL (ens1:'a multiensemble) (ens2:'a multiensemble) : 'a multiensemble =
  match ens1 with
  | [] -> [] 
  | (valu,occ)::fin ->
      if (appartientL valu ens2) then (valu,occ)::(intersectionL fin ens2)
      else intersectionL fin ens2
;;

let _ = assert(intersectionL [(1,1); (2,1); (3,1)] [(2,1); (3,1); (4,1)] = [(2,1); (3,1)])
and _ = assert(intersectionL [(1,1); (2,1)] [(2,1); (1,1)] = [(1,1); (2,1)]);;


(**
  | SPECIFICATION | differenceL
  | Profil: 'a multiensemble -> 'a multiensemble -> 'a multiensemble
  | Sémantique: (differenceL ens1 ens2) est l'ensemble des elements qui appartiennent à ens1 mais pas à ens2
  | Examples:
  |   (a) differenceL [(2,1); (1,1)] [(1,1)] = [(2,1); (1,0)]
**)

let rec differenceL (ens1:'a multiensemble) (ens2:'a multiensemble) : 'a multiensemble =
  match ens2 with
  | [] -> ens1
  | (valu,occ)::fin -> differenceL (supprimeL (valu,occ) ens1) fin
;;

let _ = assert(differenceL [(2,1); (1,1)] [(1,1)] = [(2,1); (1,0)])


(**
  | SPECIFICATION | iemL
  | Profil: 'a multiensemble -> int -> 'a
  | Sémantique: (iem ens i) renvoi un l'élément i de ens.
  | Examples:
  |   (a) (iem (A((1,2), A((13,2), A((30,2), V)))) 2) = 1 
  | REALISATION |
  | Implémentation:
**)

let rec iemL (ens: 'a multiensemble) (i: int): 'a =
	match ens with
	| [] -> 0
	| (e,occ)::ensprime -> if (occ >= i) then e else (iemL ensprime (i-occ));;

let _ = assert((iemL [(3,2); (4,1); (6,3)] 2) = 3)
and _ = assert((iemL [(3,2); (4,1); (6,3)] 3) = 4);;


(**
  | SPECIFICATION | un_dansL
  | Profil: 'a multiensemble -> 'a
  | Sémantique: (und_dans ens) renvoi un élément aléatoire de ens, en prenant en compte la repetition (plus un elt et présent, plus il a de chance d'etre renvoyé).
  | Examples:
  |   (a) un_dansL [(3,2); (4,1); (6,3)]  
  | REALISATION |
  | Implémentation:
**)

let un_dansL(ens: 'a multiensemble): 'a=
	((iemL ens (Random.int((cardinalL ens)+1))));;

let _ = (un_dansL [(3,2); (4,1); (6,3)]);;


(* Implementation des types couleur, valeur et de tuile *)

type couleur = Bleu | Rouge | Jaune | Noir;;
type valeur = int;; (* 1 à 13 *);;
type tuile = Joker | T of valeur * couleur;;


(* Implementation des types combinaison, table et pose *)

type combinaison = tuile list;;
type table = combinaison list;;
type pose = table;;


(* Implementation des types main et pioche *)


