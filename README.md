# PReTy: Pluggable Refinement Types for Scala

Current status: Very experimental

## Roadmap

### Phase 1

Implement basic foundation of refinements.

```scala
object Main {
  @refine("a > 0, b > 0, _ > 0")
  def add_positive(a: Int, b: Int): Int = if(a > b) a else b

  @refine("a > 0")
  def must_positive(a: Int) = a + 1

  def foo(): Unit = {
    must_positive(1)
    must_positive(add_positive(1, 2))
  }
}
```

* Scalac plugin
* Test
* Method refinement via annotation
* Local variable refinement via annotation
* Support limited Scala language structure
  * Object
  * Non-local method
  * Method application
  * Literals

### Phase 2+

* Support more language structure
  * Constructor
  * Control structure
  * Inner class
* Exception notation for refinement
* Path dependency
* Refine via type alias
* Higher order type refinements
* Abstract refinements
* Refinement inference for local/private methods
* Specification via proxy types
* Unsafe cast
* Compile time refinement assertions
* Dynamic checking

## References/Related works

* [Schmid, Georg Stefan and Viktor Kuncak. “SMT-based checking of predicate-qualified types for Scala.” SCALA@SPLASH (2016).](http://lara.epfl.ch/~kuncak/papers/SchmidKuncak16CheckingPredicate.pdf)
  * [src](https://github.com/gsps/dotty/tree/liquidtyper)
* [Rondon, Patrick Maxim, Ming Kawaguchi and Ranjit Jhala. “Liquid types.” PLDI (2008).](https://ranjitjhala.github.io/static/liquid_types.pdf)
* [Ranjit Jhala, Eric Seidel, Niki Vazou. "Programming with Refinement Types An Introduction to LiquidHaskell"](http://ucsd-progsys.github.io/liquidhaskell-tutorial/)
* [Ranjit Jhala, "Liquid Types for Haskell". FLOPS2014](http://goto.ucsd.edu/~rjhala/flops14/lhs/)
