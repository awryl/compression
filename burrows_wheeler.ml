(* ----------------- Encodage ------------------ *)

(* Type : 'a list -> 'a list
   Sémantique : renvoie la liste des elements decales a droite
   Exception : liste vide
   Test : rot_d ['a';'r';'b'] => ['b;'a';'r']
*)
let rot_d l =
  let rec aux = function
  |[]->failwith "liste vide"
  |[a]->([],a)
  |a::q-> let (d,c) = aux q in (a::d,c)
    in let (d,c) = aux l in
      c::d 
;;

(* Type : 'a list -> 'a list list
   Sémantique : renvoie la liste des rotations à droite
   Test : 	liste_rot [] => []
			liste_rot ['a';'r';'b'] => [['b;'a';'r'];['r';'b';'a'];['a';'r';'b']]
*)
let rec liste_rot l = 
  let n = List.length l in
  let rec aux = function
    |(0,l)->[]
    |(i,l)-> let r = rot_d l in
       r::aux(i-1,r)
  in aux (n,l)
;;

(* Type : 'a list -> 'a list
   Sémantique : tri par la methode d'insertion
*)
let tri_insertion l = 
  let rec insert e = function
    [] -> [e]
   |t::q -> if e<=t then e::t::q else t::(insert e q) in
  let rec aux = function
    [] -> []
   |t::q -> let l = aux q in insert t l
  in aux l 
;;

(* Type : 'a list list -> 'a list
   Sémantique : renvoie la liste des derniers éléments de chaque liste
   Exception : une liste est vide
   Test : all_last [] => []
		  all_last [[0;4];[7];[9;9]] => [4; 7; 9]
*)
let all_last l =
  let rec last = function
    |[]-> failwith("mot vide")
    |[a]-> a
    |a::q-> last q in
  let rec aux = function
    |[]->[]
    |a::q->last a::aux q
  in aux l
;;

(* Type : 'a -> 'a list -> int
   Sémantique : renvoie l'indice de l'element qu'on doit rechercher
   Exception : renvoie -1 si la liste de départ est vide ou si l'element n'est pas la
*)
let rec rech e = function
  |[]-> -1
  |a::q->if a=e then 1 else 1+(rech e q)
;;

(* Type : 'a list -> int * 'a list
   Sémantique : encode par la methode du burrows wheeler
   Test : encode [] => (-1, [])
		  encode [false; true; true] ;;
		  encode ['k';'i';'w';'i'] => (3, [`w`; `k`; `i`; `i`])
*)
let encode m = 
  let t = tri_insertion (liste_rot m) in
    (rech m t, all_last t)
;;

(*------------------ Decodage ----------------- *)

(* Type : 'a list -> 'a list list -> 'a list list
   Sémantique : concatene le k ieme element avec la k ieme liste
   Test : all_concat [3;5] [[8;8];[];[3]] => [[3; 8; 8]; [5]; [3]]
*)
let rec all_concat l l2 = match (l,l2) with 
  |([],[])->[]
  |([],t)->t
  |(a::q,[])->[a]::(all_concat q [])
  |(a::q,s::t)->(a::s)::(all_concat q t) 
 ;; 

(* Type : 'a list -> int -> 'a list list
   Sémantique : tri et utilise all_concat successivement sur un liste de départ l.
   Precondition : n positif
   Test : les tests sont vus directement dans le decodage
*)
let concat_successifs l n =
  let rec aux = function
    |k when k=n -> []
    |k -> all_concat l (tri_insertion (aux (k+1)))
  in tri_insertion (aux 0) 
 ;;

(* Type : int -> 'a list -> 'a
   Sémantique : Renvoie l'element d'indice i
   Exception : i est plus grand que la taille de l
   Precondition : i positif
*)
let rec rech_ind i l = match(i,l) with
  |(_,[])->failwith("indice trop grand")
  |(1,a::q)->a
  |(i,a::q)->rech_ind (i-1) q 
;;

(* Type : int * 'a list -> 'a list
   Sémantique : decode par la methode de burrows-wheeler
   Precondition : i est entre 1 et la longueur de m
   Test : decode (encode [false; true; true]) => [false; true; true]
		  decode (encode ['k';'i';'w';'i']) => ['k';'i';'w';'i']
*)
let decode (i,m) = match i with
	|(-1)->[]
	|i-> let n = List.length m in
		rech_ind i (concat_successifs m n)
;;


