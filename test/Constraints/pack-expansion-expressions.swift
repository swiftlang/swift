// RUN: %target-typecheck-verify-swift -enable-experimental-feature VariadicGenerics

// REQUIRES: asserts

func tuplify<T...>(_ t: T...) -> (T...) {
  return (repeat each t)
}

func prepend<First, Rest...>(value: First, to rest: Rest...) -> (First, Rest...) {
  return (value, repeat each rest)
}

func concatenate<T..., U...>(_ first: T..., with second: U...) -> (T..., U...) {
  return (repeat each first, repeat each second)
}

func zip<T..., U...>(_ first: T..., with second: U...) -> ((T, U)...) {
  return (repeat (each first, each second))
}

func forward<U...>(_ u: U...) -> (U...) {
  return tuplify(repeat each u)
}

func forwardAndMap<U..., V...>(us u: U..., vs v: V...) -> ([(U, V)]...) {
  return tuplify(repeat [(each u, each v)])
}

func variadicMap<T..., Result...>(_ t: T..., transform: ((T) -> Result)...) -> (Result...) {
  return (repeat (each transform)(each t))
}

func coerceExpansion<T...>(_ value: T...) {
  func promoteToOptional<Wrapped...>(_: Wrapped?...) {}

  promoteToOptional(repeat each value)
}

func localValuePack<T...>(_ t: T...) -> (T..., T...) {
  let local = repeat each t
  let localAnnotated: T... = repeat each t

  return (repeat each local, repeat each localAnnotated)
}

protocol P {
  associatedtype A

  var value: A { get }

  func f(_ self: Self) -> Self
}

func outerArchetype<T..., U>(t: T..., u: U) where T: P {
  let _: (T.A, U)... = repeat ((each t).value, u)
}

func sameElement<T..., U>(t: T..., u: U) where T: P, T == U {
  let _: T... = repeat (each t).f(u)
}

func forEachEach<C..., U>(c: C..., function: (U) -> Void)
    where C: Collection, C.Element == U {
  _ = repeat (each c).forEach(function)
}

func typeReprPacks<T...>(_ t: T...) where T: ExpressibleByIntegerLiteral {
  _ = repeat Array<each T>()
  _ = repeat 1 as each T

  _ = Array<each T>() // expected-error {{pack reference 'T' can only appear in pack expansion or generic requirement}}
  _ = 1 as each T // expected-error {{pack reference 'T' can only appear in pack expansion or generic requirement}}
}

func sameShapeDiagnostics<T..., U...>(t: T..., u: U...) {
  _ = (each t, each u)... // expected-error {{pack expansion requires that 'U' and 'T' have the same shape}}
  _ = Array<(each T, each U)>()... // expected-error {{pack expansion requires that 'U' and 'T' have the same shape}}
  _ = (Array<each T>(), each u)... // expected-error {{pack expansion requires that 'U' and 'T' have the same shape}}
}
