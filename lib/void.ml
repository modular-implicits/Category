open Categories;;

type _ void'

implicit module VoidCat : sig 
include Category with type ('a, 'b) t = ('a *'b) void' 
end = struct
  type ('a, 'b) t = ('a * 'b) void'
  
  let src : type a b. (a, b) t -> (a, a) t = function _ -> assert false (* need to use as "." syntax for unreachable does not exist *)

  let tgt : type a b. (a, b) t -> (b, b) t = function _ -> assert false

  let ( >>> ) : type a b c. (b, c) t -> (a, b) t -> (a, c) t = function _ -> assert false


end 