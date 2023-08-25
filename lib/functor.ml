open Categories;;

module type Functor = sig 
  module Dom : Category
  module Cod : Category
  type 'a ftag
  val fmap : ('a, 'b) Dom.t -> ('a ftag, 'b ftag) Cod.t
end

implicit module IdFunctor {C : Category} : Functor with type 'a ftag = 'a with module Dom := C and module Cod := C
= struct 
  module Dom = C
  module Cod = C
  type 'a ftag = 'a
  let fmap f = f
end


(* Category of categories ???*)

(*

module type CategoryOfCategories = sig 
  type ('a, 'b) t = 


*)

(* module IdBooleanCat = IdFunctor{BooleanCat} *)