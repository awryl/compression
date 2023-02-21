(***********************************************)
(***            Move to front                ***)
(***********************************************)

(** Encode une liste d'unités grâce à une fonction 
    de codage arbitraire par défaut. Pour les caractères,
    cela peut être le code ASCII par exemple
*)
val encode : ('a -> int) -> 'a list -> int list

(** Décode la liste d'unités grâce à une fonction
    de décodage par défaut. Cette fonction
    doit être la réciproque de celle utilisée
    par la fonction 'encode'
*)
val decode : (int -> 'a) -> int list -> 'a list
