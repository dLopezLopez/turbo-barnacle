#load "talf.cma";;
open Conj;;
open Auto;;
open Ergo;;
open Graf;;

let af = af_of_file "../data/ejemplo01.af";;
let cadena = [Terminal "a";Terminal "a";Terminal "a";Terminal "b";Terminal "a";Terminal "c"];;


(*****************************************************************************
 *
 * traza_af : Auto.simbolo list -> Auto.af -> bool * (Auto.estado list * string) list
 *
 *****************************************************************************)

let traza_af cadena (Af (_, _, inicial, _, finales) as a) =

   let rec aux = function

        (Conjunto [], _, pasos) ->
           (false, pasos)

      | (actuales, [], pasos) ->
           (not (es_vacio (interseccion actuales finales)),pasos)

      | (actuales, simbolo :: t, pasos) ->
           aux ((epsilon_cierre (avanza simbolo actuales a) a), t, pasos @ [((list_of_conjunto (epsilon_cierre (avanza simbolo actuales a) a)), (string_of_cadena t))])

   in
      aux ((epsilon_cierre (Conjunto [inicial]) a), cadena, [([inicial], (string_of_cadena cadena))])
   ;;

(*****************************************************************************
*
* afd_of_afn : Auto.af -> Auto.af
*
*****************************************************************************)

let finales Conjunto1 Conjunto2 = ;;

let siguientes inicial af =
  [] -> conjunto_vacio
  | h::t -> agregar (avanza h inicial af) (siguientes inicial af t)
;;


let afd_of_afn (Af (_, alfabeto, inicial, _, _) as a) =

  let rec aux = function


  in
    aux (Af((siguientes inicial a (list_of_conjunto alfabeto)),alfabeto,inicial,conjunto_vacio,conjunto_vacio),a,((siguientes inicial a (list_of_conjunto alfabeto)),agregar inicial conjunto_vacio))
;;
