open Categories;;

(* the objects are empty types *)
type fls
type tru

(* Very cool we state the morphisms as GADTs *)

type _ boolean = 
  | Fls : (fls * fls) boolean
  | F2T : (fls * tru) boolean 
  | Tru : (tru * tru) boolean

implicit module BooleanCat : sig 
  include Category with type ('a, 'b) t = ('a * 'b) boolean
  include TerminalCategory with type ('a, 'b) t := ('a, 'b) t
  include InitialCategory with type ('a, 'b) t := ('a, 'b) t
end = struct 
  type ('a, 'b) t = ('a * 'b) boolean

  let src : type a b. (a, b) t -> (a, a) t = function 
    | Fls -> Fls 
    | F2T -> Fls 
    | Tru -> Tru 

  let tgt : type a b. (a, b) t -> (b, b) t = function 
    | Fls -> Fls 
    | F2T -> Tru 
    | Tru -> Tru 

  let ( >>> ) : type a b c. (b, c) t -> (a, b) t -> (a, c) t = fun f g -> 
    match (f, g) with 
    | Fls, Fls -> Fls 
    | F2T, Fls -> F2T
    | Tru, F2T -> F2T 
    | Tru, Tru -> Tru
  
  type tObj = tru
  let terminalObject = Tru
  let terminate : type a. (a, a) t -> (a, tObj) t = function 
    | Fls -> F2T  
    | Tru -> Tru

  type iObj = fls
  let initialObject = Fls
  let initialize : type a. (a, a) t -> (iObj, a) t = function 
    | Fls -> Fls 
    | Tru -> F2T
end

(* module IdBooleanCat = BinaryCategory {BooleanCat} {BooleanCat} *)