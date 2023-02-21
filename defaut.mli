(****************************************************)
(***   Fonctions de compression / décompression   ***)
(***   par défaut. Aucune compression.            ***)
(***   Fonctionne avec toute unité.               ***)
(****************************************************)


(***********************************************)
(***   Compression/décompression  de Huffman ***)
(***********************************************)

type 'a huffman_defaut = unit

(** Construit la valeur ()
*)
val build_hfm : 'a list -> 'a huffman_defaut

(** Encode sans compression en associant directement à chaque
    valeur sa représentation mémoire interne
*)
val encode_hfm : 'a list -> 'a huffman_defaut * bool list

(** Fonction réciproque de la précédente.
    Attention à bien décoder une valeur du même type
    que la valeur encodée, sous peine d'arrêt brutal
    de l'exécution ! Une solution pour s'en assurer 
    est de typer explicitement les utilisations de 
    cette fonction, i.e. d'écrire:
    (decode_hfm ((), bl) : le_type_des_unités)
*)
val decode_hfm : 'a huffman_defaut * bool list -> 'a list


(***********************************************)
(***            Move to front                ***)
(***********************************************)

(** Encode une liste d'unités grâce à une fonction
    de codage par défaut. Aucun move-to-front n'est
    effectué, chaque unité a toujours le même code
*)
val encode_mtf : ('a -> int) -> 'a list -> int list

(** Fonction réciproque de la précédente.
*)
val decode_mtf : (int -> 'a) -> int list -> 'a list


(***********************************************)
(*****   Transformée de Burrows-Wheeler    *****)
(***********************************************)

(**
   Encode en utilisant la permutation identité.
   L'entier renvoyé est toujours 0.
*)
val encode_bw : 'a list -> int * 'a list

(**
   Fonction réciproque de la précédente.
   N'utilise pas l'entier passé en argument.
*)
val decode_bw : int * 'a list -> 'a list
