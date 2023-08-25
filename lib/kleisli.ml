
open Imp.Control;;
open Categories;;


(*
newtype Kleisli m a b = Kleisli { run :: a -> m b }

module type Kleisli = struct 
  module M = Monad 
  type ('a,'b) t = private ()'a -> 'b M.t )
end

implicit module KleisliMonad {M: Monad} = struct
  module M = M
  type ('a,'b) t = Kleisli of { run_kleisli : 'a -> 'b M.t }
end

let kleisli {K: Kleisli} x = K.Kleisli x
let run_kleisli {K: Kleisli} = K.run_kleisli

let x = kleisli f 

run_kleisli x

*)





module Kleisli (M: Monad) = struct 
  type ('a,'b) t = 'a -> 'b M.t 
end

implicit module KleisliCat {M : Monad} : Category with type ('a, 'b) t = ('a, 'b) Kleisli(M).t = struct 
  type ('a, 'b) t = ('a, 'b) Kleisli(M).t

  let src x = fun q -> M.return q
  let tgt x = fun q -> M.return q
  let ( >>> ) y x = fun a -> M.bind (x a) y
end 


