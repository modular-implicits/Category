open Categories;;

implicit module FuncCat : sig
  include Category with type ('a, 'b) t = 'a -> 'b 
end = struct 
  type ('a, 'b) t = 'a -> 'b 

  let src f = fun x -> x
  let tgt f = fun x -> x

  let ( >>> ) f g x = f (g x)
end