(***********************************************)
(*****   Transformée de Burrows-Wheeler    *****)
(***********************************************)

(**
   Encode en utilisant la transformée de Burrows-Wheeler. 
   Renvoie l'indice de position ainsi que la séquence 
   des derniers caractères 
*)
val encode : 'a list -> int * 'a list

(**
   Décode la séquence à partir de l'indice de position
   et la séquence des derniers caractères
*)
val decode : int * 'a list -> 'a list
