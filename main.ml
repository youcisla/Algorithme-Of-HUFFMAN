(* =============================== *)
(* La compression de fichiers avec *)
(*     l’algorithme de HUFFMAN     *)
(* =============================== *)

open String;;

(* ===================== *)
(* Calcul des fréquences *)
(* ===================== *)
(* définition des types  *)
(* ===================== *)

(* type freq *)
type freq = Cfreq of string * int | Ccode of string * string;;

(* type list de freqs *)
type list = Clist of freq * list | Clist_vide;;

(* type tree *)
type tree = Ctree of int * tree * tree | Cleaf of freq | Ctree_vide;;

(* type list de leafs *)
type list_tree = Clist_tree of tree * list_tree | Clist_tree_vide;;

(* ================================ *)
(* constructeur et getters de freq  *)
(* ================================ *)

(* création d'un freq constitué d'un motif et de sa fréquence. *)
let (create_freq : string -> int -> freq) = function motif -> function freq ->
  Cfreq(motif,freq);;

(* création d'un freq constitué d'un caractère et de son code binaire. *)
let (create_freq_code : string -> string -> freq ) = function caracter -> function bit ->
  Ccode(caracter,bit);;

(* renvoie le motif du freq *)
let (get_motif : freq -> string) = function Cfreq(m,_) ->
  m;;

(* renvoie la fréquence du motif *)
let (get_freq : freq -> int) = function Cfreq(_,f) ->
  f;;

let (get_caracter : freq -> string) = function Ccode(caracter, _) ->
  caracter;;

let (get_bit : freq -> string) = function Ccode(_, bit) ->
  bit;;

(* =================================== *)
(* constructeurs et getters des lists  *)
(* =================================== *)

(* crée une liste vide *)
let (create_empty_list : unit -> list) = function () -> 
  Clist_vide;;

(* test si notre liste est vide *)
let (is_empty : list -> bool) = function list ->
  list = create_empty_list();;

(* ajoute un freq dans une liste *)
let (add_frequency : freq -> list -> list) = function freq -> function list -> 
  Clist(freq, list);;

(* renvoie la premiere frequence de la liste *)
let (get_first_of_list : list -> freq) = function Clist(freq, _) -> 
  freq | _ -> failwith "get_first_of_list : Please check again";;

(* renvoie la liste sans la premiere frequence *)
let (get_rest_list : list -> list) = function Clist(_, list) -> 
  list | _ -> failwith "get_rest_list : Please check again";;

(* ============================== *)
(* getters : chaine de caractères *)
(* ============================== *)

(* test si une chain est vide *)
let (chain_is_empty : string -> bool) = function ch ->
  (ch = "");;

(* renvoie la premotifère letter d'une chaine de caractères *)
let (chain_freqrst : string -> string) = function ch ->
  sub ch 0 1;;

(* renvoie la chaine de caractère passée en paramètre privée de sa premotifère letter *)
let (chain_rest : string -> string) = function ch ->
  sub ch 1 ((length ch) - 1);;

(* ============================= *)
(* constructeurs sur les arbres *)
(* ============================ *)

let (create_tree : int -> tree -> tree -> tree) = function racine -> function left_child -> function right_child ->
  Ctree(racine, left_child, right_child);;
        
let (create_leaf : freq -> tree) = function freq ->
  Cleaf(freq);;

let (create_empty_tree : unit -> tree) = function() ->
  Ctree_vide;;

(* ====================== *)
(* getters sur les arbres *)
(* ====================== *)

let (get_leaf : tree -> freq) = function Cleaf (f) ->
  f | _ -> failwith "get_leaf : Please check again";;

(* fonction qui teste si l'élément est une feuille*)
let (is_leaf : tree -> bool) = function Cleaf(_) ->
  true | _ -> false;;

(* ajoute un arbre dans une liste d'arbre *)
let (add_tree : tree -> list_tree -> list_tree ) = function tree -> function list ->
  Clist_tree(tree, list);;

let (get_root : tree -> int) = function Ctree (r, _, _) -> 
  r | _ -> failwith "get_root : Please check again";;

let (get_left_child : tree -> tree) = function Ctree (_, left_child, _) -> 
  left_child | _ -> failwith "get_left_child : Please check again";;

let (get_right_child : tree -> tree) = function Ctree (_, _, right_child) ->
  right_child | _ -> failwith "get_right_child : Please check again";;

(* ============================================ *)
(* constructeurs et getters des listes d'arbres *)
(* ============================================ *)

(* crée une liste d'arbre vide *)
let (create_empty_trees_list : unit -> list_tree) = function () ->
  Clist_tree_vide;;

(* test si notre liste est vide *)
let (is_empty_tree : list_tree -> bool) = function list ->
  list = create_empty_trees_list();;

(* ajoute une feuille dans une liste *)
let (add_tree : tree -> list_tree -> list_tree) = function leaf -> function list_tree ->
  Clist_tree(leaf, list_tree);;

(* renvoie la premiere leaf de la liste *)
let (get_first_leaf : list_tree -> tree) = function Clist_tree(leaf, _) ->
  leaf | _ -> failwith "get_first_leaf : Please check again";;

(* renvoie la liste sans la premiere frequence *)
let (get_rest_list_tree : list_tree -> list_tree) = function Clist_tree(_, list) ->
  list | _ -> failwith "get_rest_list_tree : Please check again";;

(* ================================= *)
(* Fonctions de manipulation de base *)
(* ================================= *)

(* renvoie ma list avec le freq correspondant a la letter motifs à jour  *)
let rec (treat_element : string -> list -> list) = function letter -> function list_freqs ->
  if is_empty list_freqs then
    add_frequency (create_freq letter 1) list_freqs
  else
    if get_motif(get_first_of_list list_freqs) = letter then
      add_frequency (create_freq letter (get_freq (get_first_of_list list_freqs)+1)) (get_rest_list list_freqs)
    else if is_empty (get_rest_list list_freqs) then
      add_frequency (create_freq letter 1) list_freqs
    else
      add_frequency (get_first_of_list list_freqs) (treat_element letter (get_rest_list list_freqs));;

(* renvoie la liste des freqs d'une chaine de caratère *)
let rec (list_freq : string -> list) = function freqchier ->
  if chain_is_empty freqchier then
    create_empty_list()
  else
    treat_element (chain_freqrst freqchier) (list_freq (chain_rest freqchier ));;

(* test : *)
let list = list_freq "BCABCDCECEBCEECECCCABCECBDBDBDB";;

(* fonction qui insert un freq dans une liste *)
let rec (insert_frequency : freq -> list -> list) = function freq -> function list_freqs ->
  add_frequency freq list_freqs;;

(* fonction qui converti une listes de freq en list d'arbres *)
let rec (freq_to_tree : list -> list_tree) = function list ->
  if is_empty list then
    create_empty_trees_list()
  else
    let freq = get_first_of_list list
    and rest = get_rest_list list in
    let leaf = create_leaf freq in
    add_tree leaf (freq_to_tree rest);;

let list_tree = freq_to_tree list;;

let (get_frequence : tree -> int) = function elem ->
  if (is_leaf elem) then
    (get_freq (get_leaf elem))
  else
    get_root elem;;

(* fonction qui insert une feuille dans une liste d'arbre *)
let rec (insert_leaf : tree -> list_tree -> list_tree) = function leaf -> function list_leafs ->
  if is_empty_tree list_leafs then
    add_tree leaf list_leafs
  else
    let leaf_courant = get_first_leaf list_leafs in
    if (get_frequence leaf) <= (get_frequence leaf_courant) then
      add_tree leaf list_leafs
    else
      add_tree leaf_courant (insert_leaf leaf (get_rest_list_tree list_leafs));;

(* fonction qui prend une liste d'arbre et la renvoie trié dans l'ordre croissant *)
let rec (sort_list : list_tree -> list_tree) = function list_trees ->
  if is_empty_tree list_trees then
    create_empty_trees_list()
  else
    let leaf = get_first_leaf list_trees in
    let rest = get_rest_list_tree list_trees in
    let list_triee = sort_list rest in
    insert_leaf leaf list_triee;;

(*
let list_tree_trie = sort_list list_tree;;
*)

(* function qui affiche une listee d'arbre pour avoir un meilleur visuel *)
let rec (browse : list_tree -> string) = function list ->
  if is_empty_tree list then
    ""
  else
    let leaf = get_leaf(get_first_leaf list)
    and rest = get_rest_list_tree list in
    "("^(get_motif leaf)^", "^(string_of_int (get_freq leaf))^") "^browse rest;;

(*
browse list_tree;;
browse list_tree_trie;;
 *)

(* fonction qui insert un arbre dans une liste d'arbre *)
let rec (insert_tree : tree -> list_tree -> list_tree) = function tree -> function list_tree ->
  add_tree tree list_tree;;

let (form_tree : list_tree -> list_tree) = function list ->
  if is_empty_tree list then
    create_empty_trees_list()
  else if is_empty_tree (get_rest_list_tree list) then
    insert_tree (get_first_leaf list) list
  else
    let leaf1 = (get_first_leaf list)
    and leaf2 = (get_first_leaf (get_rest_list_tree list)) in
    
    let tree = (create_tree ((get_frequence leaf1) + (get_frequence leaf2)) leaf1 leaf2)
    and rest = get_rest_list_tree (get_rest_list_tree list) in
    insert_tree tree rest;;

(* fonction qui prend une liste d'abre et crée l'tree freqnal *)
let rec (principal_tree : list_tree -> tree) = function list ->
  let list = sort_list list in
  if (is_empty_tree (get_rest_list_tree list)) then
    get_first_leaf list
  else
    principal_tree (sort_list (form_tree list));;

let tree = principal_tree list_tree;;

let rec (concatinate_list : list -> list -> list ) = function list1 -> function list2 ->
  if is_empty list1 then
    list2
  else
    add_frequency (get_first_of_list list1) (concatinate_list (get_rest_list list1) list2);;

let rec (tree_to_binary : tree -> string -> list ) = function tree -> function bit ->
  if is_leaf tree then
    add_frequency (create_freq_code (get_motif (get_leaf tree)) bit) (create_empty_list())
  else
    concatinate_list (tree_to_binary (get_left_child tree) (bit ^ "1"))
    (tree_to_binary (get_right_child tree) (bit ^ "0"));;

(* tree_to_binary tree "";;*)

let (code : string -> list ) = function chain ->
  tree_to_binary (principal_tree(freq_to_tree (list_freq chain))) "";;

(*
code "BCABCDCECEBCEECECCCABCECBDBDBDB";;
*)

(* Fonction qui trouve la convertion binaire pour un caractère donné *)
(* list -> char -> string *)
let rec (caracter_to_binary : list -> string -> string ) = function list -> function caracter ->
  if (get_caracter (get_first_of_list list)) = caracter then
    get_bit (get_first_of_list list)
  else
    caracter_to_binary (get_rest_list list) caracter;;

(*
caracter_to_binary (code "BCABCDCECEBCEECECCCABCECBDBDBDB") "C";;
*)

let rec (code_to_binary : list -> string -> string ) = function list -> function chain ->
  if chain_is_empty chain then
    ""
  else
    (caracter_to_binary list (chain_freqrst chain)) ^ " " ^ (code_to_binary list (chain_rest chain));;
(*
code_to_binary (code "BCABCDCECEBCEECECCCABCECBDBDBDB") "BCABCDCECEBCEECECCCABCECBDBDBDB";;
*)
                                                     
let (huffman : string -> string ) = function chain ->
  let list_code = code chain in
  code_to_binary list_code chain;;                                

huffman "BCABCDCECEBCEECECCCABCECBDBDBDB";;
