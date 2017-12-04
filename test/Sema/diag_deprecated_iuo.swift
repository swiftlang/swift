// RUN: %target-typecheck-verify-swift -swift-version 3
// RUN: %target-typecheck-verify-swift -swift-version 4

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

let _: ImplicitlyUnwrappedOptional<Int> = 1 // expected-warning {{the spelling 'ImplicitlyUnwrappedOptional' is deprecated; use '!' after the type name}}{{8-36=}} {{39-39=!}} {{39-40=}}
let _: ImplicitlyUnwrappedOptional = 1 // expected-warning {{the spelling 'ImplicitlyUnwrappedOptional' is deprecated; use an explicit type followed by '!'}}

extension ImplicitlyUnwrappedOptional { // expected-warning {{using 'ImplicitlyUnwrappedOptional' in this location is deprecated and will be removed in a future release; consider changing this to 'Optional' instead}}{{11-38=Optional}}
}

func functionSpelling(
  _: ImplicitlyUnwrappedOptional<Int> // expected-warning {{the spelling 'ImplicitlyUnwrappedOptional' is deprecated; use '!' after the type name}}{{6-34=}} {{37-37=!}} {{37-38=}}
) -> ImplicitlyUnwrappedOptional<Int> { // expected-warning {{the spelling 'ImplicitlyUnwrappedOptional' is deprecated; use '!' after the type name}}{{6-34=}} {{37-37=!}} {{37-38=}}
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
  _: [Int!] // expected-warning {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}{{10-11=?}}
) -> [Int!] { // expected-warning {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}{{10-11=?}}
  return [1]
}

func genericFunction<T>(
  iuo: ImplicitlyUnwrappedOptional<T> // expected-warning {{the spelling 'ImplicitlyUnwrappedOptional' is deprecated; use '!' after the type name}}{{8-36=}} {{37-37=!}} {{37-38=}}
) -> ImplicitlyUnwrappedOptional<T> { // expected-warning {{the spelling 'ImplicitlyUnwrappedOptional' is deprecated; use '!' after the type name}}{{6-34=}} {{35-35=!}} {{35-36=}}
  return iuo
}

// Okay, like in the non-generic case.
func genericFunctionSigil<T>(
  iuo: T!
) -> T! {
  return iuo
}

func genericFunctionSigilArray<T>(
  // FIXME: We validate these types multiple times resulting in multiple diagnostics
  iuo: [T!] // expected-warning {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}{{10-11=?}}
  // expected-warning@-1 {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}{{10-11=?}}
  // expected-warning@-2 {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}{{10-11=?}}
) -> [T!] { // expected-warning {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}{{8-9=?}}
  // expected-warning@-1 {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}{{8-9=?}}
  // expected-warning@-2 {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}{{8-9=?}}
  return iuo
}

protocol P {
  associatedtype T
  associatedtype U
}

struct S : P {
  typealias T = ImplicitlyUnwrappedOptional<Int> // expected-warning {{using 'ImplicitlyUnwrappedOptional' in this location is deprecated and will be removed in a future release; consider changing this to 'Optional' instead}}{{17-45=}} {{48-48=?}} {{48-49=}}
  typealias U = Optional<ImplicitlyUnwrappedOptional<Int>> // expected-warning {{using 'ImplicitlyUnwrappedOptional' in this location is deprecated and will be removed in a future release; consider changing this to 'Optional' instead}}{{26-54=}} {{57-57=?}} {{57-58=}}

  typealias V = Int!  // expected-warning {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}{{20-21=?}}
  typealias W = Int!? // expected-warning {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}{{20-21=?}}

  var x: V
  var y: W
  var fn1: (Int!) -> Int // expected-warning {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}{{16-17=?}}
  var fn2: (Int) -> Int! // expected-warning {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}{{24-25=?}}

  subscript (
    index: ImplicitlyUnwrappedOptional<Int> // expected-warning {{the spelling 'ImplicitlyUnwrappedOptional' is deprecated; use '!' after the type name}}{{12-40=}} {{43-43=!}} {{43-44=}}
  )     -> ImplicitlyUnwrappedOptional<Int> { // expected-warning {{the spelling 'ImplicitlyUnwrappedOptional' is deprecated; use '!' after the type name}}{{12-40=}} {{43-43=!}} {{43-44=}}
    return index
  }

  subscript<T> (
    index: ImplicitlyUnwrappedOptional<T> // expected-warning {{the spelling 'ImplicitlyUnwrappedOptional' is deprecated; use '!' after the type name}}{{12-40=}} {{41-41=!}} {{41-42=}}
  )     -> ImplicitlyUnwrappedOptional<T> { // expected-warning {{the spelling 'ImplicitlyUnwrappedOptional' is deprecated; use '!' after the type name}}{{12-40=}} {{41-41=!}} {{41-42=}}
    return index
  }
}

func generic<T : P>(_: T) where T.T == ImplicitlyUnwrappedOptional<Int> { } // expected-warning {{using 'ImplicitlyUnwrappedOptional' in this location is deprecated and will be removed in a future release; consider changing this to 'Optional' instead}}{{40-68=}} {{71-71=?}} {{71-72=}}
func genericOptIUO<T : P>(_: T) where T.U == Optional<ImplicitlyUnwrappedOptional<Int>> {} // expected-warning {{using 'ImplicitlyUnwrappedOptional' in this location is deprecated and will be removed in a future release; consider changing this to 'Optional' instead}}{{55-83=}} {{86-86=?}} {{86-87=}}

func testClosure() -> Int {
  return {
    (i: ImplicitlyUnwrappedOptional<Int>) // expected-warning {{the spelling 'ImplicitlyUnwrappedOptional' is deprecated; use '!' after the type name}}{{9-37=}} {{40-40=!}} {{40-41=}}
     -> ImplicitlyUnwrappedOptional<Int> in // expected-warning {{using 'ImplicitlyUnwrappedOptional' in this location is deprecated and will be removed in a future release; consider changing this to 'Optional' instead}}{{9-37=}} {{40-40=?}} {{40-41=}}
    return i
  }(1)!
}

_ = Array<Int!>() // expected-warning {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}{{14-15=?}}
let _: Array<Int!> = [1] // expected-warning {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}{{17-18=?}}
_ = [Int!]() // expected-warning {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}{{9-10=?}}
let _: [Int!] = [1] // expected-warning {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}{{12-13=?}}
_ = Optional<Int!>(nil) // expected-warning {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}{{17-18=?}}
let _: Optional<Int!> = nil // expected-warning {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}{{20-21=?}}
_ = Int!?(0) // expected-warning {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}{{8-9=?}}
let _: Int!? = 0 // expected-warning {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}{{11-12=?}}
_ = (
  Int!, // expected-warning {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}{{6-7=?}}
  Float!, // expected-warning {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}{{8-9=?}}
  String! // expected-warning {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}{{9-10=?}}
)(1, 2.0, "3")
let _: (
  Int!, // expected-warning {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}{{6-7=?}}
  Float!, // expected-warning {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}{{8-9=?}}
  String! // expected-warning {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}{{9-10=?}}
) = (1, 2.0, "3")

struct Generic<T, U, C> {
  init(_ t: T, _ u: U, _ c: C) {}
}
_ = Generic<Int!, // expected-warning {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}{{16-17=?}}
            Float!, // expected-warning {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}{{18-19=?}}
            String!>(1, 2.0, "3") // expected-warning {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}{{19-20=?}}
let _: Generic<Int!, // expected-warning {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}{{19-20=?}}
               Float!, // expected-warning {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}{{21-22=?}}
               String!> = Generic(1, 2.0, "3") // expected-warning {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}{{22-23=?}}

func vararg(_ first: Int, more: Int!...) { // expected-warning {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}{{36-37=?}}
}

func varargIdentifier(_ first: Int, more: ImplicitlyUnwrappedOptional<Int>...) { // expected-warning {{using 'ImplicitlyUnwrappedOptional' in this location is deprecated and will be removed in a future release; consider changing this to 'Optional' instead}}{{43-71=}} {{74-74=?}} {{74-75=}}
}

func iuoInTuple() -> (Int!) { // expected-warning {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}{{26-27=?}}
  return 1
}

func iuoInTupleIdentifier() -> (ImplicitlyUnwrappedOptional<Int>) { // expected-warning {{using 'ImplicitlyUnwrappedOptional' in this location is deprecated and will be removed in a future release; consider changing this to 'Optional' instead}}{{33-61=}} {{64-64=?}} {{64-65=}}
  return 1
}

func iuoInTuple2() -> (Float, Int!) { // expected-warning {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}{{34-35=?}}
  return (1.0, 1)
}

func iuoInTuple2Identifier() -> (Float, ImplicitlyUnwrappedOptional<Int>) { // expected-warning {{using 'ImplicitlyUnwrappedOptional' in this location is deprecated and will be removed in a future release; consider changing this to 'Optional' instead}}{{41-69=}} {{72-72=?}} {{72-73=}}
  return (1.0, 1)
}

func takesFunc(_ fn: (Int!) -> Int) -> Int { // expected-warning {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}{{26-27=?}}
  return fn(0)
}

func takesFuncIdentifier(_ fn: (ImplicitlyUnwrappedOptional<Int>) -> Int) -> Int { // expected-warning {{using 'ImplicitlyUnwrappedOptional' in this location is deprecated and will be removed in a future release; consider changing this to 'Optional' instead}}{{33-61=}} {{64-64=?}} {{64-65=}}
  return fn(0)
}

func takesFunc2(_ fn: (Int) -> Int!) -> Int { // expected-warning {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}{{35-36=?}}
  return fn(0)!
}

func takesFunc2Identifier(_ fn: (Int) -> ImplicitlyUnwrappedOptional<Int>) -> Int { // expected-warning {{using 'ImplicitlyUnwrappedOptional' in this location is deprecated and will be removed in a future release; consider changing this to 'Optional' instead}}{{42-70=}} {{73-73=?}} {{73-74=}}
  return fn(0)!
}

func returnsFunc() -> (Int!) -> Int { // expected-warning {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}{{27-28=?}}
  return { $0! }
}

func returnsFuncIdentifier() -> (ImplicitlyUnwrappedOptional<Int>) -> Int { // expected-warning {{using 'ImplicitlyUnwrappedOptional' in this location is deprecated and will be removed in a future release; consider changing this to 'Optional' instead}}{{34-62=}} {{65-65=?}} {{65-66=}}
  return { $0! }
}

func returnsFunc2() -> (Int) -> Int! { // expected-warning {{using '!' in this location is deprecated and will be removed in a future release; consider changing this to '?' instead}}{{36-37=?}}
  return { $0 }
}

func returnsFunc2Identifier() -> (Int) -> ImplicitlyUnwrappedOptional<Int> { // expected-warning {{using 'ImplicitlyUnwrappedOptional' in this location is deprecated and will be removed in a future release; consider changing this to 'Optional' instead}}{{43-71=}} {{74-74=?}} {{74-75=}}
  return { $0 }
}

let x = 1 as ImplicitlyUnwrappedOptional // expected-warning {{using 'ImplicitlyUnwrappedOptional' in this location is deprecated and will be removed in a future release; consider changing this to 'Optional' instead}}{{14-41=Optional}}
let y = x!
let z: Int = x // expected-error {{value of optional type 'Int?' not unwrapped; did you mean to use '!' or '?'?}}{{15-15=!}}
