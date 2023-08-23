open Imp.Control;;
open Imp.Any;;

module type Category = sig 
  type ('a,'b) t
  val id : ('a,'a) t
  val ( |> ) : ('a,'b) t -> ('b,'c) t -> ('a,'c) t
end

let ( >>> ) {C : Category} f g = C.(g |> f)
let ( <<< ) {C : Category} f g = C.(f |> g)

implicit module Category : Category with type ('a,'b) t = 'a -> 'b 
  = struct
  type ('a,'b) t = 'a -> 'b
  let id x = x
  let ( |> ) f g x = g (f x)
end






