open Printf;;
open List;;
open String;;
(* Travail sur une lfreq de freqman *)

(* Types *)
(* ========================================================= *)

type tfreq = {car: string;
              frequence: int};;

type tlfreqsfreq = Clfreq of tfreq * tlfreqsfreq | Clfreq_vide;;

type freqORint = | Int of int | Tfreq of tfreq | CfreqORint_vide;;
 

type tExpArithm = Cexp of freqORint * freqORint * freqORint | Cexp_vide;;

(* constructeurs et sélecteurs *)
(* =========================== *)

let freqORint_vide = function () -> CfreqORint_vide;;

let cree_lfreqVide = function () -> Clfreq_vide;;

let add_freq = function freq -> function lfreq ->
  Clfreq (freq, lfreq);;

let est_vide_freq = function lfreq ->
  lfreq = cree_lfreqVide ();; 

let get_prem = function Clfreq (n, _) -> n |
  _ -> failwith "get_prem : l'argument n'a pas la forme attendue";;

let get_reste = function Clfreq (_, reste) -> reste |
  _ -> failwith "get_reste : l'argument n'a pas la forme attendue";;

(* unit -> tExpArithm - constructeur d'expression arithmétique vide *)
let c_exp_vide = function () -> Cexp_vide;;

(* string -> tExpArithm -> tExpArithm -> tExpArithm
construit une expression arithmétique à partir d'un noeud qui fera office de racine et de 2 expressions arithmétiques. *)
let (cree_exp : freqORint -> freqORint -> freqORint -> tExpArithm) = function node -> function a1 -> function a2 -> Cexp (node, a1, a2);;

(* string -> string 
renvoie la premiere lettre d'une chaine de carateres *)
let prem_lettre = function ch -> sub ch 0 1;;

(* string -> string 
renvoie la chaine de caracteres en parametre privee de sa premiere lettre *)
let reste_lettre = function ch -> sub ch 1 ((length ch) - 1);;


let est_vide = function ch ->(ch="");; 

(* ... de freq *)
(* ************ *)

let cree_freq = function ch -> function freq -> {car = ch; frequence = freq};;

let freq_vide () = CfreqORint_vide;;

(* selecteurs d'une frequence *)
let get_car = function freq -> freq.car;;
let get_frequence = function freq -> freq.frequence;;

(* let (cree_feuille : freqORint -> tExpArithm)  = function (node : freqORint) -> cree_exp node (freq_vide()) (freq_vide());; *)
let (cree_feuille : freqORint -> tExpArithm) = function node ->
  cree_exp node (freqORint_vide ()) (freqORint_vide ());;

let cfeuille = function freq -> cree_feuille (Tfreq freq);;

let get_ssag = function Cexp (_, ag, _) -> ag;;
let get_ssad = function Cexp (_, _, ad) -> ad;;
let get_r = function Cexp (r, _, _) -> r;;


let test = "BCABCDCECEBCEECECCCABCECBDBDBDB";;

let rec getLastLettre = function chaine ->
  if est_vide chaine then ""
  else if (length chaine) = 1  then prem_lettre chaine
  else 
    getLastLettre (reste_lettre chaine);;

let rec est_dans = function code -> function car ->
  if est_vide code then false 
  else let prem = prem_lettre code
    and reste = reste_lettre code
    in if car = prem then true
    else est_dans reste car;;

let rec recup_lettres = function code -> function car ->
  if est_vide code then car
  else let prem = prem_lettre code
    and reste = reste_lettre code
    in if est_dans car prem then (recup_lettres reste car)
    else recup_lettres reste (car ^ prem);; 

let test2 = recup_lettres test "";;
(* val test2 : string = "BCADE" *)

let rec nb_occurences = function code -> function car -> function cptr -> 
  if est_vide code then cptr
  else let prem = prem_lettre code
    and res = reste_lettre code 
    in if prem = car then nb_occurences res car (cptr+1)
    else nb_occurences res car cptr;;

let nboccur = nb_occurences test "A" 0;;
(* let nboccur = nb_occurences test "A" 0;; *) 

let meme_motif = function x -> function y ->
  if x = y then true else false;;

let rec majfreq = function x ->
  let prem = get_car x
  and freq = get_frequence x in cree_freq prem (freq+1);;

let rec (mise_a_jour : string -> tlfreqsfreq -> tlfreqsfreq) =
  function motif -> function lfreq ->
    if est_vide_freq lfreq 
    then add_freq (cree_freq motif 1) (cree_lfreqVide())
    else
      let prem = get_prem lfreq
      and reste = get_reste lfreq
      in if (meme_motif motif (get_car prem))
      then let new_elt = majfreq prem
        in add_freq new_elt reste
      else
        add_freq prem (mise_a_jour motif reste);;

let rec (calc_freq : string -> tlfreqsfreq) = function doc ->
  if est_vide doc then cree_lfreqVide()
  else mise_a_jour (prem_lettre doc) (calc_freq (reste_lettre doc));;
  
(* --------------------------------------- *)
  
(* Define a test string *)
let test = "BCABCDCECEBCEECECCCABCECBDBDBDB";;

(* Define a function that takes two integers and returns the maximum *)
let minnbr = fun nbr1 -> fun nbr2 ->
  if nbr1 > nbr2 then
    nbr2
  else
    nbr1;;

(* Define a function that takes two frequency records and returns the one with the highest frequency *)
let returnFilsGauche = fun freq1 -> fun freq2 ->
  let frequance1 = get_frequence freq1 and frequance2 = get_frequence freq2 in
  if (minnbr frequance1 frequance2) == frequance1 then
    freq1
  else
    freq2;;

let freqTest = cree_freq "y" 23;;
let listTest = add_freq freqTest Clfreq_vide;;

listTest;;
(*- : tlfreqsfreq = Clfreq ({car = "y"; frequence = 23}, Clfreq_vide) *)

let (oneElt:tlfreqsfreq -> tfreq) = function lfreq ->
  let reste = get_reste lfreq
  and prem = get_prem lfreq
  in if reste = Clfreq_vide
  then prem
  else failwith "there's more than one element";;

let (oneElt2:tlfreqsfreq -> bool) = function lfreq ->
  let reste = get_reste lfreq 
  in if reste = Clfreq_vide
  then true
  else false;; 

let rec (plusPetit: tlfreqsfreq -> tfreq) = function lfreq ->
  let prem = get_prem lfreq and reste = get_reste lfreq 
  in if oneElt2 lfreq then
    oneElt lfreq
  else if (oneElt2 lfreq) = false then
    returnFilsGauche prem (plusPetit reste)
  else
    plusPetit reste ;;

let rec (remove_smallest:tlfreqsfreq -> tlfreqsfreq) = function lst ->
  if oneElt2 lst 
  then Clfreq_vide
  else let smallest = plusPetit lst and hd = get_prem lst and tl = get_reste lst
    in if hd = smallest 
    then tl 
    else add_freq hd (remove_smallest tl);; 

let (add_plus_petit: tlfreqsfreq -> int) = function lfreq ->
  let freq1 = get_frequence (plusPetit lfreq) and reste = remove_smallest lfreq
  in let freq2 = get_frequence (plusPetit reste) in
  freq1 + freq2;; 

let (pre_arbre:tlfreqsfreq -> tExpArithm) = function lfreq ->
  let main = add_plus_petit lfreq
  and left = plusPetit lfreq
  and right = plusPetit (remove_smallest lfreq)
  in cree_exp (Int main) (Tfreq left) (Tfreq right);;

(*
  let rec (arbre:tlfreqsfreq -> tExpArithm) = function lfreq ->
    let main = add_plus_petit lfreq
    and left = plusPetit lfreq
    and right = plusPetit (remove_smallest lfreq)
    in if 
    then cree_exp (Int main) (Tfreq left) (Tfreq right);; *)

  

(* --------------------------------------- *)
  

(*  Tests  *)
let test = "BCABCDCECEBCEECECCCABCECBDBDBDB";;
let test2 = recup_lettres test "";;

let newlist = cree_lfreqVide ();;
let nboccur = nb_occurences test "A" 0;;
(* let nboccur = nb_occurences test "A" 0;; *)
let test3 = calc_freq test;;
(* - : tlfreqsfreq =
Clfreq ({car = "B"; frequence = 8},
 Clfreq ({car = "D"; frequence = 4},
  Clfreq ({car = "C"; frequence = 11},
   Clfreq ({car = "E"; frequence = 6},
    Clfreq ({car = "A"; frequence = 2}, Clfreq_vide))))) *) 


oneElt2 listTest;;
let exp1 = cree_freq "4" 1;;
let exp2 = cree_freq "Y" 0;;
let add= add_freq (exp1) test3;;
let bdd =add_freq (exp2) add;;
plusPetit bdd;;
remove_smallest test3;;
add_plus_petit test3;;
cree_exp (Int 1) (Int 2) (Int 3);;
cree_exp (Tfreq exp1) (Tfreq exp2) (Tfreq exp2);;
pre_arbre test3;;
(* 
- : tExpArithm =
Cexp (Int 6, Tfreq {car = "A"; frequence = 2},
 Tfreq {car = "D"; frequence = 4}) 
*)
