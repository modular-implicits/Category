
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





(*
module type CCC = sig 
include Category with type ('a, 'b) t = ('a, 'b) t
include TerminalCategory with type ('a, 'b) t := ('a, 'b) t
include InitialCategory with type ('a, 'b) t := ('a, 'b) t

end
*)