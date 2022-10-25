// RUN: %target-typecheck-verify-swift -enable-experimental-variadic-generics

func tuplify<T...>(_ t: T...) -> (T...) {
  return (t...)
}

func prepend<First, Rest...>(value: First, to rest: Rest...) -> (First, Rest...) {
  return (value, rest...)
}

func concatenate<T..., U...>(_ first: T..., with second: U...) -> (T..., U...) {
  return (first..., second...)
}

func zip<T..., U...>(_ first: T..., with second: U...) -> ((T, U)...) {
  return ((first, second)...)
}

func forward<U...>(_ u: U...) -> (U...) {
  return tuplify(u...)
}

// FIXME: This fails with opened element types. Add an ElementOf constraint to
// handle cases where the pattern type is resolved after constraint generation
// and mapped to pack archetypes in the solver.
/*
func forwardAndMap<U..., V...>(us u: U..., vs v: V...) -> ([(U, V)]...) {
  return tuplify([(u, v)]...)
}
*/

func variadicMap<T..., Result...>(_ t: T..., transform: ((T) -> Result)...) -> (Result...) {
  return (transform(t)...)
}

func coerceExpansion<T...>(_ value: T...) {
  func promoteToOptional<Wrapped...>(_: Wrapped?...) {}

  promoteToOptional(value...)
}
