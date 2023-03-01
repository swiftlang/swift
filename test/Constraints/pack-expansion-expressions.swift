// RUN: %target-typecheck-verify-swift -enable-experimental-feature VariadicGenerics

// REQUIRES: asserts

func tuplify<T...>(_ t: repeat each T) -> (repeat each T) {
  return (repeat each t)
}

func prepend<First, Rest...>(value: First, to rest: repeat each Rest) -> (First, repeat each Rest) {
  return (value, repeat each rest)
}

func concatenate<T..., U...>(_ first: repeat each T, with second: repeat each U) -> (repeat each T, repeat each U) {
  return (repeat each first, repeat each second)
}

func zip<T..., U...>(_ first: repeat each T, with second: repeat each U) -> (repeat (each T, each U)) {
  return (repeat (each first, each second))
}

func forward<U...>(_ u: repeat each U) -> (repeat each U) {
  return tuplify(repeat each u)
}

func forwardAndMap<U..., V...>(us u: repeat each U, vs v: repeat each V) -> (repeat [(each U, each V)]) {
  return tuplify(repeat [(each u, each v)])
}

func variadicMap<T..., Result...>(_ t: repeat each T, transform: repeat (each T) -> each Result) -> (repeat each Result) {
  return (repeat (each transform)(each t))
}

func coerceExpansion<T...>(_ value: repeat each T) {
  func promoteToOptional<Wrapped...>(_: repeat (each Wrapped)?) {}

  promoteToOptional(repeat each value)
}

func localValuePack<T...>(_ t: repeat each T) -> (repeat each T, repeat each T) {
  let local = repeat each t
  let localAnnotated: repeat each T = repeat each t

  return (repeat each local, repeat each localAnnotated)
}

protocol P {
  associatedtype A

  var value: A { get }

  func f(_ self: Self) -> Self
}

func outerArchetype<T..., U>(t: repeat each T, u: U) where each T: P {
  let _: repeat (each T.A, U) = repeat ((each t).value, u)
}

func sameElement<T..., U>(t: repeat each T, u: U) where each T: P, each T == U {
  let _: repeat each T = repeat (each t).f(u)
}

func forEachEach<C..., U>(c: repeat each C, function: (U) -> Void)
    where each C: Collection, each C.Element == U {
  _ = repeat (each c).forEach(function)
}

func typeReprPacks<T...>(_ t: repeat each T) where each T: ExpressibleByIntegerLiteral {
  _ = repeat Array<each T>()
  _ = repeat 1 as each T

  _ = Array<each T>() // expected-error {{pack reference 'T' can only appear in pack expansion or generic requirement}}
  _ = 1 as each T // expected-error {{pack reference 'T' can only appear in pack expansion or generic requirement}}
}

func sameShapeDiagnostics<T..., U...>(t: repeat each T, u: repeat each U) {
  _ = repeat (each t, each u) // expected-error {{pack expansion requires that 'U' and 'T' have the same shape}}
  _ = repeat Array<(each T, each U)>() // expected-error {{pack expansion requires that 'U' and 'T' have the same shape}}
  _ = repeat (Array<each T>(), each u) // expected-error {{pack expansion requires that 'U' and 'T' have the same shape}}
}

func returnPackExpansionType<T...>(_ t: repeat each T) -> repeat each T { // expected-error {{variadic expansion 'T' cannot appear outside of a function parameter list, function result, tuple element or generic argument list}}
  fatalError()
}

func returnEachPackReference<T...>(_ t: repeat each T) -> each T { // expected-error {{pack reference 'T' can only appear in pack expansion or generic requirement}}
  fatalError()
}

func returnRepeatTuple<T...>(_ t: repeat each T) -> (repeat T) { // expected-error {{pack expansion 'T' must specify a pack reference}}
  fatalError()
}

func paremeterAsPackTypeWithoutExpansion<T...>(_ t: T) -> repeat each T { // expected-error {{variadic expansion 'T' cannot appear outside of a function parameter list, function result, tuple element or generic argument list}}
  fatalError()
}

func tupleExpansion<T..., U...>(
  _ tuple1: (repeat each T),
  _ tuple2: (repeat each U)
) {
  _ = forward(repeat each tuple1.element)

  _ = zip(repeat each tuple1.element, with: repeat each tuple1.element)

  _ = zip(repeat each tuple1.element, with: repeat each tuple2.element)
  // expected-error@-1 {{global function 'zip(_:with:)' requires the type packs 'U' and 'T' have the same shape}}
}
