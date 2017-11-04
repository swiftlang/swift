// RUN: %target-typecheck-verify-swift -swift-version 5

// These are all legal uses of '!'.
struct Fine {
  var value: Int!

  func m(_ unnamed: Int!, named: Int!) -> Int! { return unnamed }
  static func s(_ unnamed: Int!, named: Int!) -> Int! { return named }

  init(_ value: Int) { self.value = value }
  init!() { return nil }

  subscript (
    index: Int!
  )     -> Int! {
    return index
  }

  subscript<T> (
    index: T!
  )     -> T! {
    return index
  }
}

let _: ImplicitlyUnwrappedOptional<Int> = 1 // expected-error {{the spelling 'ImplicitlyUnwrappedOptional' is unsupported; use '!' after the type name}}{{8-36=}}{{39-39=!}}{{39-40=}}
let _: ImplicitlyUnwrappedOptional = 1 // expected-error {{the spelling 'ImplicitlyUnwrappedOptional' in unsupported; use an explicit type followed by '!'}}

extension ImplicitlyUnwrappedOptional {} // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}

func functionSpelling(
  _: ImplicitlyUnwrappedOptional<Int> // expected-error {{the spelling 'ImplicitlyUnwrappedOptional' is unsupported; use '!' after the type name}}{{6-34=}}{{37-37=!}}{{37-38=}}
) -> ImplicitlyUnwrappedOptional<Int> { // expected-error {{the spelling 'ImplicitlyUnwrappedOptional' is unsupported; use '!' after the type name}}{{6-34=}}{{37-37=!}}{{37-38=}}
  return 1
}

// Okay, like in the method case.
func functionSigil(
  _: Int!
) -> Int! {
  return 1
}

// Not okay because '!' is not at the top level of the type.
func functionSigilArray(
  _: [Int!] // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
) -> [Int!] { // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
  return [1]
}

func genericFunction<T>(
  iuo: ImplicitlyUnwrappedOptional<T> // expected-error {{the spelling 'ImplicitlyUnwrappedOptional' is unsupported; use '!' after the type name}}{{8-36=}}{{37-37=!}}{{37-38=}}
) -> ImplicitlyUnwrappedOptional<T> { // expected-error {{the spelling 'ImplicitlyUnwrappedOptional' is unsupported; use '!' after the type name}}{{6-34=}}{{35-35=!}}{{35-36=}}
  return iuo
}

// Okay, like in the non-generic case.
func genericFunctionSigil<T>(
  iuo: T!
) -> T! {
  return iuo
}

func genericFunctionSigilArray<T>(
  iuo: [T!] // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
) -> [T!] { // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
  return iuo
}

protocol P {
  associatedtype T
  associatedtype U
}

struct S : P {
  typealias T = ImplicitlyUnwrappedOptional<Int> // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
  typealias U = Optional<ImplicitlyUnwrappedOptional<Int>> // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}

  typealias V = Int!  // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
  typealias W = Int!? // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}

  var x: V
  var y: W

  subscript (
    index: ImplicitlyUnwrappedOptional<Int> // expected-error {{the spelling 'ImplicitlyUnwrappedOptional' is unsupported; use '!' after the type name}}{{12-40=}}{{43-43=!}}{{43-44=}}
  )     -> ImplicitlyUnwrappedOptional<Int> { // expected-error {{the spelling 'ImplicitlyUnwrappedOptional' is unsupported; use '!' after the type name}}{{12-40=}}{{43-43=!}}{{43-44=}}
    return index
  }

  subscript<T> (
    index: ImplicitlyUnwrappedOptional<T> // expected-error {{the spelling 'ImplicitlyUnwrappedOptional' is unsupported; use '!' after the type name}}{{12-40=}}{{41-41=!}}{{41-42=}}
  )     -> ImplicitlyUnwrappedOptional<T> { // expected-error {{the spelling 'ImplicitlyUnwrappedOptional' is unsupported; use '!' after the type name}}{{12-40=}}{{41-41=!}}{{41-42=}}
    return index
  }
}

func generic<T : P>(_: T) where T.T == ImplicitlyUnwrappedOptional<Int> { } // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
func genericOptIUO<T : P>(_: T) where T.U == Optional<ImplicitlyUnwrappedOptional<Int>> {} // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}

func testClosure() -> Int {
  return {
    (i: ImplicitlyUnwrappedOptional<Int>) // expected-error {{the spelling 'ImplicitlyUnwrappedOptional' is unsupported; use '!' after the type name}}{{9-37=}}{{40-40=!}}{{40-41=}}
     -> ImplicitlyUnwrappedOptional<Int> in // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
    return i
  }(1)
}

_ = Array<Int!>() // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
let _: Array<Int!> = [1] // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
_ = [Int!]() // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
let _: [Int!] = [1] // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
_ = Optional<Int!>(nil) // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
let _: Optional<Int!> = nil // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
_ = Int!?(0) // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
let _: Int!? = 0 // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
_ = (
  Int!, // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
  Float!,
  String!
)(1, 2.0, "3")
let _: (
  Int!, // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
  Float!,
  String!
) = (1, 2.0, "3")

struct Generic<T, U, C> {
  init(_ t: T, _ u: U, _ c: C) {}
}
_ = Generic<Int!, // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
            Float!,
            String!>(1, 2.0, "3")
let _: Generic<Int!, // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
               Float!,
               String!> = Generic(1, 2.0, "3")

func vararg(_ first: Int, more: Int!...) { // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
}

func iuoInTuple() -> (Int!) { // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
  return 1
}

func iuoInTuple2() -> (Float, Int!) { // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
  return 1
}

func takesFunc(_ fn: (Int!) -> Int) -> Int { // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
  return fn(0)
}

func returnsFunc() -> (Int!) -> Int { // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
  return { $0 }
}
