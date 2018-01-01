# PReTy: Pluggable Refinement Types for Scala

## Roadmap

## Phase 1

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

## Phase 2+

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
