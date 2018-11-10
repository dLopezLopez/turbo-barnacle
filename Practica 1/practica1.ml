#load "talf.cma";;
open Conj;;
open Auto;;
open Ergo;;
open Graf;;


(*****************************************************************************
 *
 * traza_af : simbolo list -> af -> bool
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
