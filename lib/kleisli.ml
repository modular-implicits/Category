
open Imp.Control;;
open Categories;;


module Kleisli (M: Monad) = struct 
  type ('a,'b) t = 'a -> 'b M.t 
end

implicit module KleisliCat {M : Monad} : Category with type ('a, 'b) t = ('a, 'b) Kleisli(M).t = struct 
  type ('a, 'b) t = ('a, 'b) Kleisli(M).t

(*
  ('a, 'b) t -> ('a, 'a) t 
  ('a -> 'b M.t) -> ('a -> 'a M.t)
*)

  let src x = fun q -> M.return q
  let tgt x = fun q -> M.return q
  let ( >>> ) y x = fun a -> M.bind (x a) y

end 