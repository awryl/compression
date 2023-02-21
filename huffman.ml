(* --------------------- Encodage ----------------------- *)

type 'a huffmantree = Vide | Feuille of 'a * int | Noeud of int * 'a huffmantree * 'a huffmantree ;;

(* Type : 'a huffmantree -> int
   Sémantique : renvoie l'occurrence
   Précondition : arbre non Vide
*)
let get = function
  |Feuille(a,n)->n
  |Noeud(n,g,d)->n
;;

(* Type : 'a -> 'a list -> int
   Sémantique : renvoie l'occurence de e dans la liste
   Test : freq 8 [8;8;1;1;3] => 2
		  freq 8 [1;1;3] => 0
*) 
let rec freq e = function
  |[]->0
  |a::q when a=e -> (freq e q) +1
  |a::q->freq e q 
;;

(* Type : 'a -> 'a list -> 'a list
   Sémantique : supprime tous les e de la liste
   Test : enleve 8 [1;2;3] => [1;2;3]
		  enleve 8 [1;2;8;3;8] => [1;2;3]
*)
let rec enleve e = function
  |[]->[]
  |a::q when a=e -> enleve e q
  |a::q ->a::enleve e q 
;;

(* Type : 'a list -> 'a huffmantree list
   Sémantique : Renvoie la liste des elements et leur occurrence
   Test : ini [3;5;4;4;4] => [Feuille (3, 1); Feuille (5, 1); Feuille (4, 3)]
		  ini [1] => [Feuille (1, 1)]
*)
let rec ini = function
  |[]->[]
  |a::q->Feuille(a,(freq a q) +1)::ini (enleve a q) 
;;

(* Type : 'a huffmantree -> 'a huffmantree -> 'a huffmantree
   Sémantique : Créé le Noeud de sous arbre a et b 
				d'occurrence la somme de celui de a et celui de b
   Precondition : a et b non Vide
*)
let union a b = Noeud(get(a)+get(b),a,b) ;;

(* Type : 'a huffmantree list -> 'a huffmantree list
   Sémantique : tri la liste d'arbre l selon les occurrences
*)
let tri_insertion_arbres l = 
  let rec insert e = function
    [] -> [e]
   |t::q -> if get(e)<=get(t) then e::t::q else t::(insert e q) in
  let rec aux = function
    [] -> []
   |t::q -> let l = aux q in insert t l
  in aux l 
;;

(* Type : 'a list -> 'a huffmantree
   Sémantique : Créé l'arbre de huffman
   Test : build_tree [] => Vide
		  build_tree [5;6;9;9] =>
			Noeud (4, Noeud (2, Feuille (5, 1), Feuille (6, 1)), Feuille (9, 2))
*)
let build_tree l = 
	let rec aux = function
	|[]-> Vide
	|[a]-> a
	|a::b::q-> aux (tri_insertion_arbres ((union a b)::q))
	in aux (ini l)
;;

(* Type : 'a huffmantree -> 'a -> bool list
   Sémantique : renvoie le chemin pour aller a un element
   Precondition : arbre a non Vide
   Test : code (Feuille(5,3)) 5 => []
		  code (Noeud (3, Feuille (5, 1), Feuille (6, 2))) 6 => [true] 
*)
let code a e = 
  let rec aux = function
    |(Feuille(b,_),l)-> if b=e then l else []
    |(Noeud(n,g,d),l)-> (aux (g,false::l))@(aux (d,true::l))
  in List.rev(aux(a,[]))
;;

(* 	Type : 'a huffmantree -> 'a list -> bool list list
	Sémantique : renvoie la liste des chemins
*)
let rec code_all a = function
  |[]->[]
  |e::q->(code a e)::(code_all a q)
;;

(* Type : 'a list list -> 'a list
   Sémantique : applati la liste de liste
   Test : split [[3];[3;6;7];[7;2]] => [3; 3; 6; 7; 7; 2]
		  split [[3];[];[7;2]] => [3; 7; 2]
*)
let rec split = function
  |[]->[]
  |[]::p->split p
  |(a::q)::p->a::split(q::p) 
;;

(* Type : 'a list -> 'a huffmantree * bool list
   Sémantique : encode par la methode de huffman
   Test : encode [] => (Vide, [])
		  encode [2;3;1;3] => (Noeud (4, Feuille (1, 1), Noeud (3, Feuille (2, 1), Feuille (3, 2))),
							   [true; false; true; true; false; true; true])	
          encode [2;2] => (Feuille (2, 2), [])
*)
let encode s = 
  let a = build_tree s in
    (a,split (code_all a s))
;;

(* -------------------- Décodage ------------------*)

(* Type : 'a huffmantree * bool list -> 'a list
   Sémantique : decode par la methode de huffman
   Test : decode (Vide,[]) => []
		  decode (Feuille(2;5);[]) => [2;2;2;2;2]
		  decode (Noeud (3, Feuille (2, 1), Feuille (1, 2)),
				 [false; true; true]                          => [2;1;1] 
*)
let rec decode (a,code) = match a with
	|Vide-> []
	|Feuille(e,0) -> []
	|Feuille(e,n) -> e::(decode(Feuille(e,n-1),[]))
	|Noeud(n,g,d)->
			let rec aux = function
			|(Noeud(n,g,d),[])->failwith "suite de bits erronee"
			|(Feuille(e,n),[])->[e]
			|(Feuille(e,n),l)->e::aux(a,l)
			|(Noeud(n,g,d),false::q)->aux(g,q)
			|(Noeud(n,g,d),true::q)->aux(d,q)
			in aux(Noeud(n,g,d),code)
;;

