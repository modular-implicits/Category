
type void;;

module type Category = sig 
  type ('a, 'b) t
  
  val src : ('a, 'b) t -> ('a, 'a) t 
  val tgt : ('a, 'b) t -> ('b, 'b) t
  val ( >>> ) : ('b, 'c) t -> ('a, 'b) t -> ('a, 'c) t
end 

module type Object = sig 
  module C : Category
  type t
  val id : (t, t) C.t
end

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

module type HasBinaryProducts = sig 
include Category

  type ('a, 'b) product

  val proj1 : ('x, 'x) t -> ('y, 'y) t -> (('x, 'y) product, 'x) t
  val proj2 : ('x, 'x) t -> ('y, 'y) t -> (('x, 'y) product, 'y) t

  val ( &&& ) : ('a, 'x) t -> ('a, 'y) t -> ('a, ('x, 'y) product) t

  (* val ( *** ) : ('a1, 'b1) t -> ('a2, 'b2) t -> (('a1, 'a2) product, ('b1, 'b2) product) t *)
end 

let ( *** ) {H : HasBinaryProducts} : ('a1, 'b1) H.t -> ('a2, 'b2) H.t -> (('a1, 'a2) H.product, ('b1, 'b2) H.product) H.t = 
  fun l r -> 
    H.((l >>> (proj1 (src l) (src r))) &&& (r >>> (proj2 (src l) (src r))))


module type HasBinaryCoproducts = sig 
  include Category 
  type ('a, 'b) coprod
  val inj1 : ('x, 'x) t -> ('y, 'y) t -> ('x, ('x, 'y) coprod) t
  val inj2 : ('x, 'x) t -> ('y, 'y) t -> ('y, ('x, 'y) coprod) t

  val ( ||| ) : ('x, 'a) t -> ('y, 'a) t -> (('x, 'y) coprod, 'a) t
end 

let ( +++ ) {H : HasBinaryCoproducts} : ('a1, 'b1) H.t -> ('a2, 'b2) H.t -> (('a1, 'a2) H.coprod, ('b1, 'b2) H.coprod) H.t =
          fun l r -> 
                  H.(((inj1 (tgt l) (tgt r)) >>> l) ||| ((inj2 (tgt l) (tgt r)) >>> r))



module type Prod = sig 
  module C1' : Category
  module C2' : Category
  type _ t = Proded : (('a1, 'b1) C1'.t * ('a2, 'b2) C2'.t) -> (('a1 * 'a2) * ('b1 * 'b2)) t
end


implicit module BinaryCategory {C1 : Category} {C2 : Category} {P : Prod with module C1' = C1 and module C2' = C2} : Category with type ('a, 'b) t = ('a * 'b) P.t = struct 

  type ('a, 'b) t = ('a * 'b) P.t

  let src : type a b . (a, b) t -> (a, a) t = function
    | P.Proded (l, r) -> P.Proded (C1.src l, C2.src r)

  let tgt : type a b. (a, b) t -> (b, b) t = function 
    | P.Proded (l, r) -> P.Proded (C1.tgt l, C2.tgt r)

  let ( >>> ) : type a b c. (b, c) t -> (a, b) t -> (a, c) t = fun f g -> 
    match f, g with 
    | P.Proded (l, r), P.Proded (l', r') -> Proded (C1.(l >>> l'), C2.(r >>> r'))
end


module type Prod' = sig 
  module C1' : Category
  module C2' : Category
  type _ t = Proded : (('a1, 'b1) C1'.t * ('a2, 'b2) C2'.t) -> (('a1 * 'a2) * ('b1 * 'b2)) t
  val proj1 : (('a1 * 'a2) * ('b1 * 'b2)) t -> ('a1, 'a2) C1'.t
  val proj2 : (('a1 * 'a2) * ('b1 * 'b2)) t -> ('b1, 'b2) C2'.t
end

(*
implicit module BinaryCategory2 {C1 : Category} {C2 : Category} : sig 
  include Prod' with module C1' = C1 and module C2' = C2
  include Category with type ('a, 'b) t := ('a * 'b) Prod'.t
end = struct


  type _ t = Proded : (('a1, 'b1) C1.t * ('a2, 'b2) C2.t) -> (('a1 * 'a2) * ('b1 * 'b2)) t

  (*
  let proj1 : type a b. (a * b) t -> (a, b) C1.t = function
    | Proded (l, _) -> l

  let proj2 : type a b. (a * b) t -> (a, b) C2.t = function
    | Proded (_, r) -> r
    *)
end
*)


(* Solid option number 2*)
module type ProductCategoryType = sig 
  type ('a1, 'b1) t1
  type ('a2, 'b2) t2
  type _ t' = Proded : (('a1, 'b1) t1 * ('a2, 'b2) t2) -> (('a1 * 'a2) * ('b1 * 'b2)) t'
end


implicit module BinaryCategory4 {C1 : Category} {C2 : Category} : sig 
  include ProductCategoryType with type ('a1, 'b1) t1 = ('a1, 'b1) C1.t and type ('a2, 'b2) t2 = ('a2, 'b2) C2.t
  include Category with type ('a, 'b) t = ('a * 'b) t'
end = struct 

  type ('a1, 'b1) t1 = ('a1, 'b1) C1.t
  type ('a2, 'b2) t2 = ('a2, 'b2) C2.t
  type _ t' = Proded : (('a1, 'b1) t1 * ('a2, 'b2) t2) -> (('a1 * 'a2) * ('b1 * 'b2)) t'
  type ('a, 'b) t = ('a * 'b) t'

  let src : type a b . (a, b) t -> (a, a) t = function
    | Proded (l, r) -> Proded (C1.src l, C2.src r)

  let tgt : type a b. (a, b) t -> (b, b) t = function 
    | Proded (l, r) -> Proded (C1.tgt l, C2.tgt r)

  let ( >>> ) : type a b c. (b, c) t -> (a, b) t -> (a, c) t = fun f g -> 
    match f, g with 
    | Proded (l, r), Proded (l', r') -> Proded (C1.(l >>> l'), C2.(r >>> r'))
end


(*


implicit module BinaryCategory {C1 : Category} {C2 : Category} : Category with type ('a, 'b) t = ((('a1, 'a2) C1.t) * (('b1, 'b2) C2.t)) constraint ('a * 'b) = ((('a1, 'a2) C1.t) * (('b1, 'b2) C2.t)) = struct 


  type ('a, 'b) t = ((('a1, 'a2) C1.t) * (('b1, 'b2) C2.t)) constraint ('a * 'b) = ((('a1, 'a2) C1.t) * (('b1, 'b2) C2.t))

  let src = function
    | (l, r) -> (C1.src l, C2.src r)

  let tgt = function
    | (l, r) -> (C1.tgt l, C2.tgt r)

  let ( >>> ) f g = 
    match f, g with 
    | (l, r), (l', r') -> (C1.(l >>> l'), C2.(r >>> r'))
end
*)