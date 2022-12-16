// RUN: %target-typecheck-verify-swift -enable-experimental-feature VariadicGenerics

// REQUIRES: asserts

func tuplify<T...>(_ t: T...) -> (T...) {
  return ((each t)...)
}

func prepend<First, Rest...>(value: First, to rest: Rest...) -> (First, Rest...) {
  return (value, (each rest)...)
}

func concatenate<T..., U...>(_ first: T..., with second: U...) -> (T..., U...) {
  return ((each first)..., (each second)...)
}

func zip<T..., U...>(_ first: T..., with second: U...) -> ((T, U)...) {
  return ((each first, each second)...)
}

func forward<U...>(_ u: U...) -> (U...) {
  return tuplify((each u)...)
}

func forwardAndMap<U..., V...>(us u: U..., vs v: V...) -> ([(U, V)]...) {
  return tuplify([(each u, each v)]...)
}

func variadicMap<T..., Result...>(_ t: T..., transform: ((T) -> Result)...) -> (Result...) {
  return ((each transform)(each t)...)
}

func coerceExpansion<T...>(_ value: T...) {
  func promoteToOptional<Wrapped...>(_: Wrapped?...) {}

  promoteToOptional((each value)...)
}

func localValuePack<T...>(_ t: T...) -> (T..., T...) {
  let local = (each t)...
  let localAnnotated: T... = (each t)...

  return ((each local)..., (each localAnnotated)...)
}

protocol P {
  associatedtype A

  var value: A { get }

  func f(_ self: Self) -> Self
}

func outerArchetype<T..., U>(t: T..., u: U) where T: P {
  let _: (T.A, U)... = ((each t).value, u)...
}

func sameElement<T..., U>(t: T..., u: U) where T: P, T == U {
  let _: T... = (each t).f(u)...
}

func forEachEach<C..., U>(c: C..., function: (U) -> Void)
    where C: Collection, C.Element == U {
  _ = (each c).forEach(function)...
}
