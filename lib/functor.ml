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

(* G F (x))*)

implicit module ComposeFunctor {G : Functor} {F : Functor with module Cod := G.Dom} : Functor with type 'a ftag = 'a F.ftag G.ftag with module Dom := F.Dom and module Cod := G.Cod
= struct 
  module Dom = F.Dom
  module Cod = G.Cod
  type 'a ftag = ('a F.ftag) G.ftag
  let fmap f = G.fmap (F.fmap f)
end

implicit module ConstFunctor {C : Category} {D : Category} {O : Object with module C = D} : Functor with type 'a ftag = O.t with module Dom := C and module Cod := D
= struct 
  module Dom = C
  module Cod = D
  type 'a ftag = O.t
  let fmap f = O.id_arr
end


(* Category of categories ???*)

(*

module type CategoryOfCategories = sig 
  type ('a, 'b) t = 


*)

(* module IdBooleanCat = IdFunctor{BooleanCat} *)

(*


data Cat :: (Type -> Type -> Type) -> (Type -> Type -> Type) -> Type where
  CatA :: (Functor ftag, Category (Dom ftag), Category (Cod ftag)) => ftag -> Cat (Dom ftag) (Cod ftag)


-- | @Cat@ is the category with categories as objects and funtors as arrows.
instance Category Cat where

  src (CatA _)      = CatA Id
  tgt (CatA _)      = CatA Id

  CatA f1 . CatA f2 = CatA (f1 :.: f2)

*)