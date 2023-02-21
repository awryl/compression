(***********************************************)
(***   Compression/décompression  de Huffman ***)
(***********************************************)

(** Type de l'arbre binaire utilisé pour coder le document
*)
type 'a huffmantree

(** Prend une liste d'élément et construit l'arbre de codage
*)
val build_tree : 'a list -> 'a huffmantree

(** Prend une liste d'éléments et renvoie le couple 
    (arbre de codage de huffman, codage binaire de la liste)
*)
val encode : 'a list -> 'a huffmantree * bool list

(** Prend un arbre de codage de huffman et une liste de booléens 
    et reconstruit la liste d'éléments décodée
*)
val decode : 'a huffmantree * bool list -> 'a list
