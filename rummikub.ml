(* Implémentation des notions de multielement *)

type nat = int (* >= 0 *)
type 'a multielement = 'a * nat (* 'a == reservoir des valeurs *)


(* Implémentation des notions de multiensemble sur type somme *)

type 'a multiensemble =
|V											(* V pour Vide *)
|A of 'a multielement * 'a multiensemble	(* A pour Ajout *)


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
  | A ((e, nbr), ensprime) -> (appartient e ens2) && (inclus ensprime ens2)
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

let elt_of (elt, _ : 'a multielement) : 'a = elt;;
let occ_of (_, occ : 'a multielement) : int = occ;;

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

let elt_of (elt, _ : 'a multielement) : 'a = elt;;
let occ_of (_, occ : 'a multielement) : int = occ;;

let rec supprime (elt: 'a multielement) (ens: 'a multiensemble) : 'a multiensemble =
	match ens with
	| V -> V
	| A ((e, o), ens) -> if (e = (elt_of elt)) then A((e, o - (occ_of elt)), ens) else A ((e, o), (supprime elt ens))
;;

let _ = assert(supprime (3,2) (A((3,2), A((4,1), A((6,3), V)))) = A((3,0), A((4,1), A((6,3), V))));;



