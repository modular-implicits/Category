




module type Category = sig 
  type ('a, 'b) t
  
  val src : ('a, 'b) t -> ('a, 'a) t 
  val tgt : ('a, 'b) t -> ('b, 'b) t
  val ( >>> ) : ('b, 'c) t -> ('a, 'b) t -> ('a, 'c) t
end 


(* the objects are empty types *)
type fls
type tru

(* Very cool we state the morphisms as GADTs *)

type _ boolean = 
  | Fls : (fls * fls) boolean
  | F2T : (fls * tru) boolean 
  | Tru : (tru * tru) boolean



implicit module OpCat {C : Category} : Category with type ('a, 'b) t = ('b, 'a) C.t = struct 
  type ('a, 'b) t = ('b, 'a) C.t

  let src = C.tgt
  let tgt = C.src

  let ( >>> ) : type a b c. (b, c) t -> (a, b) t -> (a, c) t = fun f g -> 
    C.(g >>> f)
  end 

module type TerminalCategory = sig 
  include Category 
  type tObj
  val terminalObject : (tObj, tObj) t
  val terminate : ('a, 'a) t -> ('a, tObj) t
end

module type InitialCategory = sig 
  include Category
  type iObj
  val initialObject : (iObj, iObj) t
  val initialize : ('a, 'a) t -> (iObj, 'a) t
end


implicit module BooleanCat : sig 
  include Category with type ('a, 'b) t = ('a * 'b) boolean
  include TerminalCategory with type ('a, 'b) t := ('a, 'b) t
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
end

implicit module FuncCat : sig
  include Category with type ('a, 'b) t = 'a -> 'b 
end = struct 
  type ('a, 'b) t = 'a -> 'b 

  let src f = fun x -> x
  let tgt f = fun x -> x

  let ( >>> ) f g x = f (g x)
end