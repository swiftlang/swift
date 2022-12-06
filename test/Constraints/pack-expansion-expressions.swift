// RUN: %target-typecheck-verify-swift -enable-experimental-feature VariadicGenerics

// REQUIRES: asserts

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

func forwardAndMap<U..., V...>(us u: U..., vs v: V...) -> ([(U, V)]...) {
  return tuplify([(u, v)]...)
}

func variadicMap<T..., Result...>(_ t: T..., transform: ((T) -> Result)...) -> (Result...) {
  return (transform(t)...)
}

func coerceExpansion<T...>(_ value: T...) {
  func promoteToOptional<Wrapped...>(_: Wrapped?...) {}

  promoteToOptional(value...)
}

func localValuePack<T...>(_ t: T...) -> (T..., T...) {
  let local = t...
  let localAnnotated: T... = t...

  return (local..., localAnnotated...)
}

protocol P {
  associatedtype A

  var value: A { get }

  func f(_ self: Self) -> Self
}

func outerArchetype<T..., U>(t: T..., u: U) where T: P {
  let _: (T.A, U)... = (t.value, u)...
}

func sameElement<T..., U>(t: T..., u: U) where T: P, T == U {
  let _: T... = t.f(u)...
}

func forEachEach<C..., U>(c: C..., function: (U) -> Void)
    where C: Collection, C.Element == U {
  _ = c.forEach(function)...
}
