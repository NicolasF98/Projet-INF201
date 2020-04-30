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

let rec iem (i: int) (ens: 'a multiensemble): 'a =
	match ens with
	| V -> 0
	| A((e,occ),ensprime) -> if (occ >= i) then e else (iem (i-occ) ensprime);;

let _ = assert((iem 2 (A((3,2), A((4,1), A((6,3), V))))) = 3)
and _ = assert((iem 3 (A((3,2), A((4,1), A((6,3), V))))) = 4);;


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
	((iem (Random.int((cardinal ens)+1)) ens));;

let _ = (un_dans(A((3,2), A((4,1), A((6,3), V)))));;



(* PARTIE Q2: REUSINAGE DU CODE - UTILISATION DU TYPE LISTE *)

(* PS: Pour différencer les fonctions reusiné, le suffixe L serra ajouté à chaque nom de fonction *)


(* Redefinition du type multiensemble *)

type 'a multiensemble2 = 'a multielement list;;


(**
  | SPECIFICATION | cardinalL
  | Profil: 'a multielement list -> int
  | Sémantique: (cardinalL ens) est le nombre d'elements de ens.
  | Examples:
  |   (a) cardinalL [ (2, 5) ;(3, 1) ] = 6
  |   (b) cardinalL [ (10, 3) ] = 3
  |   (c) cardinalL [ (true, 2); (false, 2) ] = 4
*)

let rec cardinalL (ens:'a multiensemble2) : int=
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

let rec nboccL (elt: 'a) (ens:'a multiensemble2) : int =
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

let rec appartientL (elt:'a) (ens:'a multiensemble2) : bool =
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

let rec inclusL (ens1:'a multiensemble2) (ens2:'a multiensemble2) : bool=
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

let ajouteL (elt:'a multielement) (ens: 'a multiensemble2): 'a multiensemble2 =
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

let rec supprimeL (elt:'a multielement) (ens: 'a multiensemble2): 'a multiensemble2 =
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

let egauxL (ens1:'a multiensemble2) (ens2: 'a multiensemble2) : bool =
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

let rec intersectionL (ens1:'a multiensemble2) (ens2:'a multiensemble2) : 'a multiensemble2 =
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

let rec differenceL (ens1:'a multiensemble2) (ens2:'a multiensemble2) : 'a multiensemble2 =
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

let rec iemL (i: int)(ens: 'a multiensemble2): 'a =
  match ens with
  | [] -> failwith "pas de iem elt dans une liste vide"
  | (a,b)::s -> if i <= b then a else (iemL (i-b) s)
  ;; 


(**
  | SPECIFICATION | un_dansL
  | Profil: 'a multiensemble -> 'a
  | Sémantique: (und_dans ens) renvoi un élément aléatoire de ens, en prenant en compte la repetition (plus un elt et présent, plus il a de chance d'etre renvoyé).
  | Examples:
  |   (a) un_dansL [(3,2); (4,1); (6,3)]  
  | REALISATION |
  | Implémentation:
**)

let un_dansL(ens: 'a multiensemble2): 'a=
	((iemL (Random.int((cardinalL ens)+1)) ens));;

let _ = (un_dansL [(3,2); (4,1); (6,3)]);;

(* RÉUSINAGE DU CODE - ORDRE SUPÉRIEUR *)

(* PS: Pour différencer les fonctions reusiné, le suffixe T serra ajouté à chaque nom de fonction *)


(**
  | SPECIFICATION | cardinalT
  | Profil: 'a multielement list -> int
  | Sémantique: (cardinalT ens) est le nombre d'elements de ens.
  | Examples:
  |   (a) cardinalT [ (2, 5) ;(3, 1) ] = 6
  |   (b) cardinalT [ (10, 3) ] = 3
  |   (c) cardinalT [ (true, 2); (false, 2) ] = 4
*)

let rec cardinalT(ens:'a multiensemble2):int= (* >=0 *)
  match ens with
  | []->0 
  |(n,m)::suite ->  List.fold_left (+) m [cardinalT (suite)];; 
     

(**
  | SPECIFICATION | nombre d'occurrences
  | Profil: 'a -> 'a multiensemble -> int
  | Sémantique: (nboccT elt ens) nombre d'occurrence de elt dans ens.
  | Examples:
  | 	(a) nboccT 6 [ (2,1); (6,3) ] = 3
**)

let rec nboccT(el:'a)(ens:'a multiensemble2):int = (* >= 0*) 
  match ens with 
  |[] -> 0 
  |(autre,nb)::suite -> 
      if autre == el then nb 
      else (nboccT el (List.tl ens)) ;;


(**
  | SPECIFICATION | appartientT
  | Profil: 'a -> 'a multiensemble -> bool
  | Sémantique: (appartientT elt ens) est vrai si et seulement si elt appartient à ens
  | Examples:
  |   (a) appartientT 2 [(2,1); (3,2); (10,1)] = true
  |   (b) appartientT 4 [(2,1); (3,1); (4,0)] = false
*)

let appartientT(elt:'a)(ensemble: 'a multiensemble2):bool = 
  List.mem (elt,(nboccT elt ensemble)) ensemble;;


(**
  | SPECIFICATION | inclusT
  | Profil: 'a multiensemble -> 'a multiensemble -> bool
  | Sémantique: (inclusT ens1 ens2) est vrai si et seulement si ens1 est inclus dans ens2
  | Examples:
  |   (a) inclusT [(1,2); (2,3); (3,3)] [(0,1); (1,2); (2,3); (3,3)] = true
  |   (b) inclusT [(0,1)] [(1,2); (2,3); (3,3)] = false
 *)

let rec inclusT(ensemble1:'a multiensemble2)(ensemble2:'a multiensemble2):bool= 
  if (cardinalT ensemble2)==0 && (cardinalT ensemble1)==0 then true
  else if (cardinalT ensemble2)==0 then false
  else
    match ensemble1 with
    |[]->true
    |(n,m)::suite -> if List.mem (n,m) ensemble2 then (inclusT suite ensemble2)
        else false;;


(**
  | SPECIFICATION | ajouteT
  | Profil: 'a multielement -> 'a multiensemble -> 'a multiensemble
  | Sémantique: (ajouteT elt ens) est l'ensemble obtenu en ajoutant l'élément elt à l'ensemble ens en respectant la contrainte 
                de non répétition des éléments.
  | Examples:
  |   (a) ajouteT (7,1) [(0,1); (1,1); (2,1); (3,1); (4,1); (5,1)] = [(0,1); (1,1); (2,1); (3,1); (4,1); (5,1); (7,1)] 
  |   (b) ajouteT (1,1) [(0,1); (1,1); (2,1); (3,1); (4,1); (5,1)] =  [(0,1); (1,1); (2,1); (3,1); (4,1); (5,1)]
*)

let rec ajouteT((n,m):'a multielement)(ens:'a multiensemble2):'a multiensemble2= 
  if m==0 then ens else
    match ens with 
    |[] -> List.append [(n,m)] []
    |(x,nb)::suite ->if not(appartientT n ens) then List.append [(n,m)] ens 
        else 
        if x != n then List.append [(x,nb)] (ajouteT (n,m) suite) 
        else List.append[(n,m+nb)] suite;;


(**
  | SPECIFICATION | supprimeT
  | Profil: 'a multiensemble -> 'a multiensemble -> 'a multiensemble
  | Sémantique: (supprimeT elt ens) supprime l'élément elt de l'ensemble ens.
  | Examples:
  |   (a) supprimeT (3,1) [(1,1); (2,2); (3,1)] = [(1,1); (2,2)]
  |   (c) supprimeT (2,1) [(1,1); (2,2); (3,1)] = [(1,1); (2,1); (3,1)]
*)

let rec supprimeT((x,n):'a multielement)(ens:'a multiensemble2):'a multiensemble2=
  match ens with
  |[] -> []
  |(el,nb)::suite -> if not(appartientT x ens) then ens 
      else if x != el then List.append[(el,nb)] (supprimeT (x,n) suite)
      else if nb>n then List.append [(x,nb-n)] suite else suite;;


(**
  | SPECIFICATION | intersectionT
  | Profil: 'a multiensemble -> 'a multiensemble -> 'a multiensemble
  | Sémantique: (intersectionT ens1 ens2) est l'ensemble qui appartient à la fois à ens1 et ens2.
  | Examples:
  |   (a) intersectionT [(1,1); (2,1); (3,1)] [(2,1); (3,1); (4,1)] = [(2,1); (3,1)]
  |   (c) intersectionT [(1,1); (2,1)] [(2,1); (1,1)] = [(1,1); (2,1)]
**)

let rec intersectionT(ens1:'a multiensemble2)(ens2:'a multiensemble2):'a multiensemble2 = 
  match ens1 with
  |[] -> []
  |(n1,m1)::s1 -> match ens2 with
    |[] -> []
    |(n2,m2)::s2 -> 
        if ((nboccT n1 ens2)>m1) 
        then (ajouteT (n1,m1) (intersectionT s1 ens2))
        else (ajouteT (n1,(nboccT n1 ens2)) (intersectionT s1 ens2));;


(**
  | SPECIFICATION | differenceT
  | Profil: 'a multiensemble -> 'a multiensemble -> 'a multiensemble
  | Sémantique: (differenceT ens1 ens2) est l'ensemble des elements qui appartiennent à ens1 mais pas à ens2
  | Examples:
  |   (a) differenceT [(2,1); (1,1)] [(1,1)] = [(2,1); (1,0)]
**)

let rec differenceT(ens1:'a multiensemble2)(ens2:'a multiensemble2):'a multiensemble2 = 
  if (ens1 == []) then ens1
  else
    match ens2 with 
    |[]->ens1
    |(n,m)::s->
        if ((nboccT n ens1)>m) 
        then (supprimeT (n,m) (differenceT ens1 s))
        else (supprimeT (n,(nboccT n ens1)) (differenceT ens1 s));;


(* Implémentation des types couleur, valeur, tuile, combinaison, table, pose, main, pioche *)
type couleur = Bleu | Rouge | Jaune | Noir ;;
type valeur = nat;; (* 1 à 13 *)
type tuile = 
  |Joker
  |T of valeur * couleur;;

  type combinaison = tuile list;;
type table = combinaison list;;
type pose = combinaison list;; 

type main = tuile multiensemble2;;
type pioche = tuile multiensemble2;;

let cst_PIOCHE_INIT : pioche = 
  [ (Joker,2) ; T(1,Rouge), 2 ;  T(2,Rouge), 2 ; T(3,Rouge), 2 ;
    T(4,Rouge), 2 ; T(5,Rouge), 2 ; T(6,Rouge), 2 ; T(7,Rouge), 2 ;
    T(8,Rouge), 2 ; T(9,Rouge), 2 ; T(10,Rouge), 2 ; T(11,Rouge), 2 ;
    T(12,Rouge), 2 ; T(13,Rouge), 2 ;
    T(1,Bleu), 2 ;  T(2,Bleu), 2 ; T(3,Bleu), 2 ;
    T(4,Bleu), 2 ; T(5,Bleu), 2 ; T(6,Bleu), 2 ; T(7,Bleu), 2 ;
    T(8,Bleu), 2 ; T(9,Bleu), 2 ; T(10,Bleu), 2 ; T(11,Bleu), 2 ;
    T(12,Bleu), 2 ; T(13,Bleu), 2 ;
    T(1,Jaune), 2 ;  T(2,Jaune), 2 ; T(3,Jaune), 2 ;
    T(4,Jaune), 2 ; T(5,Jaune), 2 ; T(6,Jaune), 2 ; T(7,Jaune), 2 ;
    T(8,Jaune), 2 ; T(9,Jaune), 2 ; T(10,Jaune), 2 ; T(11,Jaune), 2 ;
    T(12,Jaune), 2 ; T(13,Jaune), 2 ;
    T(1,Noir), 2 ;  T(2,Noir), 2 ; T(3,Noir), 2 ;
    T(4,Noir), 2 ; T(5,Noir), 2 ; T(6,Noir), 2 ; T(7,Noir), 2 ;
    T(8,Noir), 2 ; T(9,Noir), 2 ; T(10,Noir), 2 ; T(11,Noir), 2 ;
    T(12,Noir), 2 ; T(13,Noir), 2 ];;          


(**
  | SPECIFICATION | en_ordre
  | Profil: pioche -> pioche
  | Sémantique: (en_ordre p) range dans l'ordre la pioche fourni en arguement
  | Utilisation de 2 fonctions annexe, range_joker, range_couleur.
**)

let rec range_joker(n:tuile multiensemble2):tuile multiensemble2 = 
  match n with
  |[] -> []
  |(Joker,nb)::autre -> (autre)@[Joker,nb]
  |(T(v,c),m)::autre -> [T(v,c),m]@(range_joker autre)
;;  


let rec range_couleur(c:couleur)(n:tuile multiensemble2):tuile multiensemble2 =
  match (range_joker n) with
  |[] -> []
  |(Joker,nb)::autre -> if c == Noir then [Joker,nb] else (range_couleur c autre)
  |(T(v,coul),m)::autre -> if coul == c then [T(v,coul),m]@(range_couleur c autre)  else (range_couleur c autre);;


let en_ordre (n:pioche):pioche = 
  (List.sort compare (range_couleur Bleu n))@
  (List.sort compare (range_couleur Rouge n))
  @(List.sort compare (range_couleur Jaune n))@
  (range_joker(List.sort compare (range_couleur Noir n))) 
;;


(* Implémenation des types joueur, statut, etat *)

type joueur = J1 | J2;;
type statut = joueur * bool * main;;
type etat = (statut * statut) * table * pioche * joueur;; 


(**
  | SPECIFICATION | extraire
  | Profil: nat -> pioche -> main*pioche
  | Sémantique: (extraire n p) extrait n carte aléatoirement de la pioche p, et renvoi la main de n carte, et la pioche restante.
  | Utilisation de 1 fonctions annexe, extraire_main.
**)

let rec extraire_main(n:nat)(p:pioche):main= 
  match n with
  |0-> [] 
  |_-> [(un_dansL p),1]@(extraire_main (n-1) (differenceT p [(un_dansL p),1]));;


let extraire (n:nat)(p:pioche):main*pioche =
  let main=(extraire_main n p) in
  (main, (differenceT p main));; 


(**
  | SPECIFICATION | distrib
  | Profil: -> main*main*pioche
  | Sémantique: (distrib()) génére depuis la pioche inital 1 main pour deux joueurs.
**)  

let distrib():main*main*pioche=
  let main1,pioche = (extraire 14 cst_PIOCHE_INIT) in
  let main2,nlle_p = (extraire 14 pioche) in
  (main1,main2,nlle_p);;


(**
  | SPECIFICATION | init_partie
  | Profil: -> etat
  | Sémantique: (init_partie()) défini un etat initiale après la distribution des cartes
**) 

let init_partie():etat=
  let (main1,main2,pioche)=(distrib ()) in 
  ((J1,false,main1),(J2,false,main2)),[],pioche,J1;;


(* Implémentation des fonctions d'accès *)

let joueur_courant (e:etat): joueur =
  let ((stat, stat1), table1, pioche1, jou) = e
  in (jou);;

  
let joueur_suivant (e:etat): joueur =
  let ((stat, stat1), table1, pioche1, jou) = e
  in if jou == J1 then J2 else J1;;


let la_table (e:etat):table=
  let ((stat, stat1), table1, pioche1, jou) = e
  in (table1);;


let la_pioche (e:etat):pioche=
  let ((stat, stat1), table1, pioche1, jou) = e
  in (pioche1);;

(*
let la_main (j:joueur)(e:etat):main=
  let ((stat, stat1), table1, pioche1, jou) = e
  in [(stat, stat1)];;
*)
let p1:combinaison = [T(5,Noir);  T(5,Noir); T(5,Noir)];;
let p2:combinaison = [T(12,Noir);  T(12,Bleu); T(12,Rouge)];;


(**
  | SPECIFICATION | est_suite
  | Profil: combinaison -> bool
  | Sémantique: (est_suite p) renvoi true si la combinaison est une suite valide, false si ce n'est pas le cas.
**)

let est_suite (c:combinaison):bool=
  if List.length(c) < 3 then false else
  let (T(nv,coul1)::T(nv2,coul2)::T(nv3,coul3)::autre) = c
  in if (coul1==coul2 && coul2==coul3) &&(nv<=nv2 && nv2<=nv3) then true else false
  ;;


(**
  | SPECIFICATION | est_groupe
  | Profil: combinaison -> bool
  | Sémantique: (est_groupe p) renvoi true si la combinaison est une groupe valide, false si ce n'est pas le cas.
**)

let est_groupe (c:combinaison):bool=
  if List.length(c) < 3 then false else
  let (T(nv,coul1)::T(nv2,coul2)::T(nv3,coul3)::autre) = c
  in if (coul1!=coul2 && coul2!=coul3) &&(nv==nv2 && nv2==nv3) then true else false 
  ;;


(**
  | SPECIFICATION | combinaison_valide
  | Profil: combinaison -> bool
  | Sémantique: (combinaison_valide p) renvoi true si la combinaison est un groupe ou une suite valide, false si ce n'est pas le cas.
**)

let combinaison_valide (c:combinaison):bool=
  if ((est_suite c) == true) || ((est_groupe c) == true) then true else false
  ;;


(**
  | SPECIFICATION | points
  | Profil: combinaison -> int
  | Sémantique: (points p) renvoi la valeur en point de la combinaison fournie en argument, si cette combinaison est vide elle vaut 0.
**)

let rec points(c:combinaison):int=
  match c with
  | [] -> 0
  | Joker::autre -> (points autre) + 30
  | T(valu,coul)::autre -> (points autre) + valu
  ;;


(**
  | SPECIFICATION | points_suite
  | Profil: combinaison -> int
  | Sémantique: (points_suite p) renvoi la valeur en point de la combinaison fourni en argument,seulement si c'est une suite, sinon elle vaut 0.
**)

let points_suite (c:combinaison):int=
  if (est_suite c == true) then points c else 0
  ;;


(**
  | SPECIFICATION | points_groupe
  | Profil: combinaison -> int
  | Sémantique: (points_groupe p) renvoi la valeur en point de la combinaison fourni en argument,seulement si c'est un groupe, sinon elle vaut 0.
**)

let points_groupe (c:combinaison):int=
  if (est_groupe c == true) then points c else 0
    ;;
