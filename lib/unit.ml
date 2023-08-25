open Categories;;

type _ single = Single : (unit * unit) single

implicit module Singleton : sig 
include  Category with type ('a, 'b) t = ('a * 'b) single
include TerminalCategory with type ('a, 'b) t := ('a, 'b) t
include InitialCategory with type ('a, 'b) t := ('a, 'b) t
include HasBinaryProducts with type ('a, 'b) t := ('a, 'b) t
include HasBinaryCoproducts with type ('a, 'b) t := ('a, 'b) t
end = struct 
  type ('a, 'b) t = ('a * 'b) single
  
  let src : type a b. (a, b) t -> (a, a) t = function 
    | Single -> Single

  let tgt : type a b. (a, b) t -> (b, b) t = function 
    | Single -> Single

  let ( >>> ) : type a b c. (b, c) t -> (a, b) t -> (a, c) t = fun f g ->
    match f, g with 
    | Single, Single -> Single

  type tObj = unit 
  let terminalObject = Single
  let terminate : type a. (a, a) t -> (a, tObj) t = function 
    | Single -> Single
  
  type iObj = unit
  let initialObject = Single
  let initialize : type a. (a, a) t -> (iObj, a) t = function 
    | Single -> Single

  type ('a, 'b) product = unit
  let proj1 : type a b . (a, a) t -> (b, b) t -> ((a, b) product, a) t = fun f g ->
    match f, g with 
    | Single, Single -> Single

  let proj2 : type a b . (a, a) t -> (b, b) t -> ((a, b) product, b) t = fun f g ->
    match f, g with 
    | Single, Single -> Single

  let ( &&& ) : type a b c. (a, b) t -> (a, c) t -> (a, (b, c) product) t = fun f g ->
    match f, g with 
    | Single, Single -> Single

  type ('a, 'b) coprod = unit
  let inj1 : type a b . (a, a) t -> (b, b) t -> (a, (a, b) coprod) t = fun f g ->
    match f, g with 
    | Single, Single -> Single
  
  let inj2 : type a b . (a, a) t -> (b, b) t -> (b, (a, b) coprod) t = fun f g ->
    match f, g with 
    | Single, Single -> Single

  let ( ||| ) : type a b c. (a, b) t -> (c, b) t -> ((a, c) coprod, b) t = fun f g ->
    match f, g with 
    | Single, Single -> Single
end