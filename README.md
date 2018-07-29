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

## Type checking scheme

```
# Value
v := r     # root value
   | v(id) # property

# Pred
p := (e, { id: p }) # Pred for the value and its properties

# Expr
e := _             # "the value"
   | e^+
e^+ := expr_value(v) # value reference
     | other expressions ...

# Logic
l := forall {v}. l
   | e^+

# Environment
E := ({ r: p }, {v1}, {v2})
  # Root value bindings: Value `r` has pred `p`.
  # Boolean values {v1} is true.
  # Boolean values {v2} is false.
```

```
# Given environment E, p1 <: p2
satisfy(E = (bindings, cond, uncond), p1, p2): Boolean =
  solve_SMT(embed(E, p1 <: p2)) == SAT


# Substitute
e[e1/e2] = Substitute sub-expr e1 in e to e2

all_values(p1=(e1, ps1), p2=(e2, ps2)) =
  values(e1) ++ values(e2) ++
    { all_values(ps1(id), ps2(id)) | id in keys(ps2) union keys(ps2) if modified(ps1, id) || modified(ps2, id) }

embed(E=(bindings, conds, unconds)), p1 <: p2) =
    forall {r | (r: p) in E}.
      { embed(v: pred1(bindings, v)) | v in all_values(p1, p2) }
      && { expr_value(v) == true | v in conds \hat all_values(p1, p2) }
      && { expr_value(v) == false | v in unconds \hat all_values(p1, p2) }
      && embed(p1 <: p2)

embed(p1=(e1, ps1) <: p2=(e2, ps2)): Logic =
  let diffs = { id | (id, _) in ps2 where ps1(id) != ps2(id) } in
    forall v=fresh().
      embed(v: e1) --> embed(v: e2)
      && { embed(ps1(id) <: ps2(id)) | id in diffs }

embed(v: e): Logic =
  e[_/v]

pred1(bindings, v): Expr =
  let (e, _) = pred(bindings, v) in e

pred(bindings, v): Pred = v match {
  v(id) => let (_, ps) = pred(bindings, v) in ps(id)
  r => bindings(r)
}
```
