# PReTy: Pluggable Refinement Types for Scala

Current status: Very experimental

## Roadmap


* Local variable refinement via annotation
* Control structure with path dependency
* Constructor
* Inner class
* Exception notation for refinement
* Refine via type alias
* Higher order type refinements
* Abstract refinements
* Refinement inference for local/private methods
* Specification via proxy types
* Unsafe cast
* Compile time refinement assertions
* Dynamic checking

## References/Related works

### Refinement Types and Liquid Types
* [Schmid, Georg Stefan and Viktor Kuncak. “SMT-based checking of predicate-qualified types for Scala.” SCALA@SPLASH (2016).](http://lara.epfl.ch/~kuncak/papers/SchmidKuncak16CheckingPredicate.pdf)
  * [Implementation](https://github.com/gsps/dotty/tree/liquidtyper)
* [Rondon, Patrick Maxim, Ming Kawaguchi and Ranjit Jhala. “Liquid types.” PLDI (2008).](https://ranjitjhala.github.io/static/liquid_types.pdf)
* [Ranjit Jhala, Eric Seidel, Niki Vazou. "Programming with Refinement Types An Introduction to LiquidHaskell"](http://ucsd-progsys.github.io/liquidhaskell-tutorial/)
* [Ranjit Jhala, "Liquid Types for Haskell". FLOPS2014](http://goto.ucsd.edu/~rjhala/flops14/lhs/)
* [Refined: Refinement types library for Scala](https://github.com/fthomas/refined)

### Design of Scala Internals
* [Fengyun Liu, Eugene Burmako. "Two Approaches to Portable Macros", Technical Report, July, 2017, EPFL](https://infoscience.epfl.ch/record/231413/files/gestalt.pdf)
* [Burmako, Eugene et al. “Unification of Compile-Time and Runtime Metaprogramming in Scala.” (2016).](https://infoscience.epfl.ch/record/226166/files/EPFL_TH7159.pdf)
