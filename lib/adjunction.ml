
open Categories;;
open Functor;;

module type Adjunction = sig
  module C : Category
  module D : Category
  module F : Functor with module Dom = D and module Cod = C
  module G : Functor with module Dom = C and module Cod = D
end



implicit module Adjunction (C' : Category) (D' : Category) (F' : Functor with module Dom = D' and module Cod = C') (G' : Functor with module Dom = C' and module Cod = D') : sig
include Adjunction with module C = C' and module D = D' and module F = F' and module G = G'
end  = struct
  module C = C'
  module D = D'
  module F = F'
  module G = G'

end
