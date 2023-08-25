open Categories;;

implicit module FuncCat : sig
  include Category with type ('a, 'b) t = 'a -> 'b 
  include TerminalCategory with type ('a, 'b) t := ('a, 'b) t
  include InitialCategory with type ('a, 'b) t := ('a, 'b) t
  include HasBinaryProducts with type ('a, 'b) t := ('a, 'b) t
  include HasBinaryCoproducts with type ('a, 'b) t := ('a, 'b) t
end = struct 
  type ('a, 'b) t = 'a -> 'b 

  let src f = fun x -> x
  let tgt f = fun x -> x

  let ( >>> ) f g x = f (g x)

  type tObj = unit
  let terminalObject = fun _ -> ()
  let terminate f = fun _ -> ()

  type iObj = void
  let initialObject = fun x -> x
  let rec initialize x = initialize x

  type ('a, 'b) product = 'a * 'b
  let proj1 x y = fun (x', y') -> x'
  let proj2 x y = fun (x', y') -> y'
  let ( &&& ) f g x = (f x, g x)

  type ('a, 'b) coprod = Left of 'a | Right of 'b
  let inj1 x y = fun x' -> Left x'
  let inj2 x y = fun y' -> Right y'
  let ( ||| ) f g x = match x with
    | Left x' -> f x'
    | Right y' -> g y'

end