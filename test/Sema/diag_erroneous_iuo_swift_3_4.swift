// RUN: %target-typecheck-verify-swift -swift-version 3
// RN: %target-typecheck-verify-swift -swift-version 4

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
  _: [Int!] // expected-warning {{implicitly unwrapped optionals are only allowed at top level and as function results; treating '!' as if it were '?' for Swift 3 and Swift 4}}
) -> [Int!] { // expected-warning {{implicitly unwrapped optionals are only allowed at top level and as function results; treating '!' as if it were '?' for Swift 3 and Swift 4}}
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
  // FIXME: We validate these lists multiple times and end up emitting the warning each time.
  iuo: [T!] // expected-warning {{implicitly unwrapped optionals are only allowed at top level and as function results; treating '!' as if it were '?' for Swift 3 and Swift 4}}
  // expected-warning@-1 {{implicitly unwrapped optionals are only allowed at top level and as function results; treating '!' as if it were '?' for Swift 3 and Swift 4}}
  // expected-warning@-2 {{implicitly unwrapped optionals are only allowed at top level and as function results; treating '!' as if it were '?' for Swift 3 and Swift 4}}
  // FIXME: We validate these lists multiple times and end up emitting the warning each time.
) -> [T!] { // expected-warning {{implicitly unwrapped optionals are only allowed at top level and as function results; treating '!' as if it were '?' for Swift 3 and Swift 4}}
// expected-warning@-1 {{implicitly unwrapped optionals are only allowed at top level and as function results; treating '!' as if it were '?' for Swift 3 and Swift 4}}
// expected-warning@-2 {{implicitly unwrapped optionals are only allowed at top level and as function results; treating '!' as if it were '?' for Swift 3 and Swift 4}}
  return iuo
}

protocol P {
  associatedtype T
  associatedtype U
}

struct S : P {
  typealias T = ImplicitlyUnwrappedOptional<Int> // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
  typealias U = Optional<ImplicitlyUnwrappedOptional<Int>> // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}

  typealias V = Int!  // expected-warning {{implicitly unwrapped optionals are only allowed at top level and as function results; treating '!' as if it were '?' for Swift 3 and Swift 4}}
  typealias W = Int!? // expected-warning {{implicitly unwrapped optionals are only allowed at top level and as function results; treating '!' as if it were '?' for Swift 3 and Swift 4}}

  var x: V
  var y: W
  var fn1: (Int!) -> Int // expected-warning {{implicitly unwrapped optionals are only allowed at top level and as function results; treating '!' as if it were '?' for Swift 3 and Swift 4}}
  var fn2: (Int) -> Int! // expected-warning {{implicitly unwrapped optionals are only allowed at top level and as function results; treating '!' as if it were '?' for Swift 3 and Swift 4}}

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

_ = Array<Int!>() // expected-warning {{implicitly unwrapped optionals are only allowed at top level and as function results; treating '!' as if it were '?' for Swift 3 and Swift 4}}
let _: Array<Int!> = [1] // expected-warning {{implicitly unwrapped optionals are only allowed at top level and as function results; treating '!' as if it were '?' for Swift 3 and Swift 4}}
_ = [Int!]() // expected-warning {{implicitly unwrapped optionals are only allowed at top level and as function results; treating '!' as if it were '?' for Swift 3 and Swift 4}}
let _: [Int!] = [1] // expected-warning {{implicitly unwrapped optionals are only allowed at top level and as function results; treating '!' as if it were '?' for Swift 3 and Swift 4}}
_ = Optional<Int!>(nil) // expected-warning {{implicitly unwrapped optionals are only allowed at top level and as function results; treating '!' as if it were '?' for Swift 3 and Swift 4}}
let _: Optional<Int!> = nil // expected-warning {{implicitly unwrapped optionals are only allowed at top level and as function results; treating '!' as if it were '?' for Swift 3 and Swift 4}}
_ = Int!?(0) // expected-warning {{implicitly unwrapped optionals are only allowed at top level and as function results; treating '!' as if it were '?' for Swift 3 and Swift 4}}
let _: Int!? = 0 // expected-warning {{implicitly unwrapped optionals are only allowed at top level and as function results; treating '!' as if it were '?' for Swift 3 and Swift 4}}
_ = (
  Int!, // expected-warning {{implicitly unwrapped optionals are only allowed at top level and as function results; treating '!' as if it were '?' for Swift 3 and Swift 4}}
  Float!, // expected-warning {{implicitly unwrapped optionals are only allowed at top level and as function results; treating '!' as if it were '?' for Swift 3 and Swift 4}}
  String! // expected-warning {{implicitly unwrapped optionals are only allowed at top level and as function results; treating '!' as if it were '?' for Swift 3 and Swift 4}}
)(1, 2.0, "3")
let _: (
  Int!, // expected-warning {{implicitly unwrapped optionals are only allowed at top level and as function results; treating '!' as if it were '?' for Swift 3 and Swift 4}}
  Float!, // expected-warning {{implicitly unwrapped optionals are only allowed at top level and as function results; treating '!' as if it were '?' for Swift 3 and Swift 4}}
  String! // expected-warning {{implicitly unwrapped optionals are only allowed at top level and as function results; treating '!' as if it were '?' for Swift 3 and Swift 4}}
) = (1, 2.0, "3")

struct Generic<T, U, C> {
  init(_ t: T, _ u: U, _ c: C) {}
}
_ = Generic<Int!, // expected-warning {{implicitly unwrapped optionals are only allowed at top level and as function results; treating '!' as if it were '?' for Swift 3 and Swift 4}}
            Float!, // expected-warning {{implicitly unwrapped optionals are only allowed at top level and as function results; treating '!' as if it were '?' for Swift 3 and Swift 4}}
            String!>(1, 2.0, "3") // expected-warning {{implicitly unwrapped optionals are only allowed at top level and as function results; treating '!' as if it were '?' for Swift 3 and Swift 4}}
let _: Generic<Int!, // expected-warning {{implicitly unwrapped optionals are only allowed at top level and as function results; treating '!' as if it were '?' for Swift 3 and Swift 4}}
               Float!, // expected-warning {{implicitly unwrapped optionals are only allowed at top level and as function results; treating '!' as if it were '?' for Swift 3 and Swift 4}}
               String!> = Generic(1, 2.0, "3") // expected-warning {{implicitly unwrapped optionals are only allowed at top level and as function results; treating '!' as if it were '?' for Swift 3 and Swift 4}}

func vararg(_ first: Int, more: Int!...) { // expected-warning {{implicitly unwrapped optionals are only allowed at top level and as function results; treating '!' as if it were '?' for Swift 3 and Swift 4}}
}

func varargIdentifier(_ first: Int, more: ImplicitlyUnwrappedOptional<Int>...) { // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
}

func iuoInTuple() -> (Int!) { // expected-warning {{implicitly unwrapped optionals are only allowed at top level and as function results; treating '!' as if it were '?' for Swift 3 and Swift 4}}
  return 1
}

func iuoInTupleIdentifier() -> (ImplicitlyUnwrappedOptional<Int>) { // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
  return 1
}

func iuoInTuple2() -> (Float, Int!) { // expected-warning {{implicitly unwrapped optionals are only allowed at top level and as function results; treating '!' as if it were '?' for Swift 3 and Swift 4}}
  return (1.0, 1)
}

func iuoInTuple2Identifier() -> (Float, ImplicitlyUnwrappedOptional<Int>) { // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
  return (1.0, 1)
}

func takesFunc(_ fn: (Int!) -> Int) -> Int { // expected-warning {{implicitly unwrapped optionals are only allowed at top level and as function results; treating '!' as if it were '?' for Swift 3 and Swift 4}}
  return fn(0)
}

func takesFuncIdentifier(_ fn: (ImplicitlyUnwrappedOptional<Int>) -> Int) -> Int { // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
  return fn(0)
}

func takesFunc2(_ fn: (Int) -> Int!) -> Int { // expected-warning {{implicitly unwrapped optionals are only allowed at top level and as function results; treating '!' as if it were '?' for Swift 3 and Swift 4}}
  return fn(0) // expected-error {{value of optional type 'Int?' not unwrapped; did you mean to use '!' or '?'?}}
}

func takesFunc2Identifier(_ fn: (Int) -> ImplicitlyUnwrappedOptional<Int>) -> Int { // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
  return fn(0)
}

func returnsFunc() -> (Int!) -> Int { // expected-warning {{implicitly unwrapped optionals are only allowed at top level and as function results; treating '!' as if it were '?' for Swift 3 and Swift 4}}
  return { $0 } // expected-error {{cannot convert value of type 'Int?' to closure result type 'Int'}}
}

func returnsFuncIdentifier() -> (ImplicitlyUnwrappedOptional<Int>) -> Int { // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
  return { $0 }
}

func returnsFunc2() -> (Int) -> Int! { // expected-warning {{implicitly unwrapped optionals are only allowed at top level and as function results; treating '!' as if it were '?' for Swift 3 and Swift 4}}
  return { $0 }
}

func returnsFunc2Identifier() -> (Int) -> ImplicitlyUnwrappedOptional<Int> { // expected-error {{implicitly unwrapped optionals are only allowed at top level and as function results}}
  return { $0 }
}
