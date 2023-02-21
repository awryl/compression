(* --------------- Encodage ----------------*)

(* 	Type : 'a -> ('a * 'b) list -> bool
	Sémantique : Renvoie si oui ou non l'element fait parti des 1ers éléments de la liste
*)
let rec est_dedans e = function
	|[]->false
	|a::q-> fst(a)=e || (est_dedans e q)
;;

(* 	Type : ('a -> 'b) -> 'a list -> ('a * 'b) list
	Sémantique : cree la table des element ainsi que leur indice
	Test : c_creer_tab Char.code [`a`;`t`;`k`;`a`] =>
				[`t`, 116; `k`, 107; `a`, 97]
*)
let rec c_creer_tab f = function
	|[]->[]
	|a::q -> let t = c_creer_tab f q in
		if est_dedans a t then t else (a,f a)::t
;;

(* 	Type : 'a -> ('a * 'b) list -> 'b
	Sémantique : renvoie le second du couple en parcourant la liste
*)
let rec second c = function
	|[]->failwith "problème de recherche"
	|(a,n)::q->if a=c then n else second c q
;;

(* 	Type : int -> ('a * int) list -> ('a * int) list
	Sémantique : renvoie la table engendre par le décalage
	Test : c_etape_suiv 107 [`t`, 116; `k`, 107; `a`, 97] =>
				[`t`, 116; `k`, 0; `a`, 98]
*)
let rec c_etape_suiv p = function
	|[]->[]
    |(a,n)::q-> let l = c_etape_suiv p q in 
				if n=p then (a,0)::l
				else if n<p then (a,n+1)::l
				else (a,n)::l
;; 

(* 	Type : ('a * int) list -> 'a list -> int list
	Sémantique : renvoie la table engendré par des decalages successifs
	Test : les tests sont faits dans encode
*)
let rec c_mtf l = function
	|[]->[]
	|a::q-> let p = second a l in
			p::(c_mtf (c_etape_suiv p l) q) 
;;

(* 	Type : ('a -> int) -> 'a list -> int list
	Sémantique : encode par le move to front
	Test : encode Char.code ['k';'i';'w';'i'] => [107; 106; 119; 1]
		   encode Char.code [] => []
*)
let encode f m = c_mtf (c_creer_tab f m) m ;;

(*-------------- Décodage ---------------*)

(* 	Type : int list -> int
	Sémantique : renvoie l'element maximum
	Exception : renvoie 0 si la liste est vide
*)
let rec max = function
	|[]-> 0
	|[a]->a
	|a::q-> let m = max q in 
			if a>m then a else m
;;

(*Toutes les fonctions suivantes sont quasiment les memes, ainsi les tests
   ne sont pas necessaires *)
   
(* 	Type : int -> (int * 'a) list -> (int * 'a) list
	Sémantique : renvoie la table engendre par le décalage
*)
let rec d_etape_suiv p = function
	|[]->[]
    |(n,a)::q-> let l = d_etape_suiv p q in 
				if n=p then (0,a)::l
				else if n<p then (n+1,a)::l
				else (n,a)::l
;; 

(* 	Type : (int * 'a) list -> int list -> 'a list
	Sémantique : renvoie la table engendré par des decalages successifs
*)
let rec d_mtf l = function
	|[]->[]
	|n::q-> let a = second n l in
			a::(d_mtf (d_etape_suiv n l) q) 
;;

(* 	Type : (int -> 'a) -> int -> (int * 'a) list
	Sémantique : cree la table des indices avec leur element
*)
let d_creer_tab f max =
	let rec aux = function
	|m when m=max+1 -> []
	|m->(m,f m)::(aux (m+1))
	in aux 0
;;

(* 	Type : (int -> 'a) -> int list -> 'a list
	Sémantique : decode par le move to front
	Test : decode Char.chr (encode Char.code ['k';'i';'w';'i']) 
				=> ['k';'i';'w';'i']
		   decode Char.chr [] -> []
*)
let decode f m = d_mtf (d_creer_tab f (max m)) m ;;
