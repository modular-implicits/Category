Category Theory Library

The library relies on types representing objects, this is done using GADTs, as an example:

```ocaml
type _ boolean = 
  | Fls : (fls * fls) boolean
  | F2T : (fls * tru) boolean 
  | Tru : (tru * tru) boolean
```

Above we display all the morphisms as tuples, where each object in the boolean category is either `fls` or `tru`.

As types cannot be represented as values the identity morphisms are used in their place to represent objects at 
the value level, you can get these representations of objects with the following functions:

```ocaml
val src : ('a, 'b) t -> ('a, 'a) t 
val tgt : ('a, 'b) t -> ('b, 'b) t
```







