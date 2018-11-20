#load "talf.cma";;
open Conj;;
open Auto;;
open Ergo;;
open Graf;;

let af = af_of_file "../data/ejemplo01.af";;
let afn = af_of_file "../data/afn.af";;
let ej24 = af_of_file "../data/ejercicio24.af";;
let ej25 = af_of_file "../data/ejercicio25.af";;
let ej26 = af_of_file "../data/ejercicio26.af";;
let ej26b = af_of_file "../data/ejercicio26b.af";;
let estados_afn = (Conjunto [(Conjunto [Estado "0"],Estado "0");(Conjunto [Estado "1";Estado "2"],Estado "1");(Conjunto [Estado "2"],Estado "2");(Conjunto [Estado "0";Estado "2"],Estado "3") ; (conjunto_vacio, Estado "4") ]);;
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

let rec primero_que_cumple f = function
  [] ->   raise Not_found
  | h::t -> match f h with
    true -> h
    | false -> primero_que_cumple f t
;;

let existe f = function
  [] -> false
  | l -> match
          try Some (primero_que_cumple f l) with
            Not_found -> None
          with None -> false
          | Some (_) -> true
;;

let asociado l k =
  match primero_que_cumple (function (n,_) -> igual n k) l with
  (_,m) -> m
;;

(*Devuelve un conjunto de Estados a los que se avanza para cada simbolo del alfabeto*)
let rec siguientes inicial af = function
  [] -> conjunto_vacio
  | h::t -> agregar (epsilon_cierre (avanza h inicial af) af) (siguientes inicial af t)
;;
(*Devuelve un conjunto de Estados que tienen un estado que es final*)
let finales estados_afd estados_finales =
  let rec aux = function
    ([],_) -> []
    | (h::t,estados_finales) ->
      let rec recorrido = function
        (_,[]) -> []
        | ((c,e), h::t) -> match pertenece h c with
                      true -> e :: recorrido ((c,e),t)
                      | false -> recorrido ((c,e),t)
      in recorrido(h,estados_finales) @ aux(t,estados_finales)
  in aux (list_of_conjunto estados_afd, list_of_conjunto estados_finales)
;;
(*Devuelve los arcos entre el conjunto de estados c*)
let arcos c (Af (_, alfabeto, _, _, _) as a) =
  let rec aux = function
    ([],_,_,_) -> conjunto_vacio
    | (h::t,alfabeto,a,l) ->
      let rec recorrido = function
        (_,[],_,_) -> conjunto_vacio
        | ((c,e),h::t,a,l) -> agregar (Arco_af(e,asociado l (epsilon_cierre(avanza h (epsilon_cierre c a) a) a),h)) (recorrido ((c,e),t,a,l))
      in union (recorrido (h,alfabeto,a,l)) (aux (t,alfabeto,a,l))
  in aux(list_of_conjunto c,list_of_conjunto alfabeto,a,list_of_conjunto c)
;;

let rec estados_afd = function
  [] -> conjunto_vacio
  | (_,e)::t -> agregar e (estados_afd t)
;;

let afd_of_afn (Af (_, alfabeto, inicial, _, _) as a) =

  let rec aux = function
  (Af (_, alfabeto, _, _, estados_finales) as a,([],conjunto_tratado),_) -> Af((estados_afd (list_of_conjunto conjunto_tratado)), alfabeto, Estado "0", (arcos conjunto_tratado a), conjunto_of_list (finales conjunto_tratado estados_finales))
  | (Af (_, alfabeto, _, _, _) as a,(h::f,conjunto_tratado),n) ->
    match existe (function (x,_) -> igual x h) (list_of_conjunto conjunto_tratado) with
      false -> aux(a,(list_of_conjunto (union (conjunto_of_list f) (siguientes h a (list_of_conjunto alfabeto))), agregar (h,Estado (string_of_int n)) conjunto_tratado),n+1)
      | true -> aux(a, ( f, conjunto_tratado),n)
  in
    aux (a ,( list_of_conjunto (siguientes (epsilon_cierre (Conjunto [inicial]) a) a (list_of_conjunto alfabeto)), Conjunto  [(Conjunto [inicial], Estado "0")]) ,1)

;;

let afd_of_afne =
  afd_of_afn
;;
