multidimensional-map
====================

Multidimensional finite maps, with heterogeneous indices collected into vinyl records.  This requires type-level strings and such from GHC 7.6.

This library is experimental at best; I'm toying with implementation things and new features.  (Quiz: which is faster, `GADTMMap`, in which every `Map` is wrapped in a GADT, or `NestedMMap`, which uses associated newtype families to eliminate the intermediary constructors?)

A map like `exampleMMap :: GADTMMap '["field1" ::: String, "field2" ::: String] Int` is analogous to (and is implemented as) `Map String (Map String Int)`.


Currently the only special feature, other than the usual typeclass instances, is the class function `applyMap`.  It's perhaps a bit cryptic, and definitely needs a better name.  It allows an intersection with a single layer of the MMap, preserving the other layers.  (The "dimension" analogy breaks down a bit here.)  

So if we had `exampleMMap` as above, with values like
  ╔════════╦════════╦═══════╗
  ║ field1 ║ field2 ║ value ║
  ╠════════╬════════╬═══════╣
  ║ "x"    ║ "a"    ║     2 ║
  ║ "x"    ║ "b"    ║     3 ║
  ║ "y"    ║ "a"    ║     5 ║
  ║ "z"    ║ "a"    ║     7 ║
  ╚════════╩════════╩═══════╝

and a flat map, `exampleFactors :: Map String (Int -> Int)` with values like

  ╔═════╦════════╗
  ║ key ║ value  ║
  ╠═════╬════════╣
  ║ "x" ║ (* 11) ║
  ║ "a" ║ (* 17) ║
  ╚═════╩════════╝

we could apply it to either `"field1"` or `"field2"`.  In pseudo-interactive-mode (there's no `Show` instance, much less one that makes pretty tables:

  > applyMap ("field1" ::: String) exampleFactors exampleMMap
  ╔════════╦════════╦═══════╗
  ║ field1 ║ field2 ║ value ║
  ╠════════╬════════╬═══════╣
  ║ "x"    ║ "a"    ║    22 ║
  ║ "x"    ║ "b"    ║    33 ║
  ╚════════╩════════╩═══════╝

  > applyMap ("field2" ::: String) exampleFactors exampleMMap
  ╔════════╦════════╦═══════╗
  ║ field1 ║ field2 ║ value ║
  ╠════════╬════════╬═══════╣
  ║ "x"    ║ "a"    ║    34 ║
  ║ "y"    ║ "a"    ║    85 ║
  ║ "z"    ║ "a"    ║   119 ║
  ╚════════╩════════╩═══════╝

In actual use cases, it would probably not make sense to apply the same map to different dimensions; I'm only doing so here to illustrate the library semantics.
