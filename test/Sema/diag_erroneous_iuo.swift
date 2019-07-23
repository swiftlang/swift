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
let _: ImplicitlyUnwrappedOptional<Int> = 1 // expected-error {{'ImplicitlyUnwrappedOptional' has been renamed to 'Optional'}}{{8-35=Optional}}
let _: ImplicitlyUnwrappedOptional = 1 // expected-error {{'ImplicitlyUnwrappedOptional' has been renamed to 'Optional'}}

extension ImplicitlyUnwrappedOptional {} // expected-error {{'ImplicitlyUnwrappedOptional' has been renamed to 'Optional'}}

func functionSpelling(
  _: ImplicitlyUnwrappedOptional<Int> // expected-error {{'ImplicitlyUnwrappedOptional' has been renamed to 'Optional'}}{{6-33=Optional}}
) -> ImplicitlyUnwrappedOptional<Int> { // expected-error {{'ImplicitlyUnwrappedOptional' has been renamed to 'Optional'}}{{6-33=Optional}}
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
  _: [Int!] // expected-error {{'!' is not allowed here; perhaps '?' was intended?}}{{10-11=?}}
) -> [Int!] { // expected-error {{'!' is not allowed here; perhaps '?' was intended?}}{{10-11=?}}
  return [1]
}

func genericFunction<T>(
  iuo: ImplicitlyUnwrappedOptional<T> // expected-error {{'ImplicitlyUnwrappedOptional' has been renamed to 'Optional'}}{{8-35=Optional}}
) -> ImplicitlyUnwrappedOptional<T> { // expected-error {{'ImplicitlyUnwrappedOptional' has been renamed to 'Optional'}}{{6-33=Optional}}
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
  iuo: [T!] // expected-error {{'!' is not allowed here; perhaps '?' was intended?}}{{10-11=?}}
  // expected-error@-1 {{'!' is not allowed here; perhaps '?' was intended?}}{{10-11=?}}
) -> [T!] { // expected-error {{'!' is not allowed here; perhaps '?' was intended?}}{{8-9=?}}
  // expected-error@-1 {{'!' is not allowed here; perhaps '?' was intended?}}{{8-9=?}}
  return iuo
}

protocol P {
  associatedtype T // expected-note {{protocol requires nested type 'T'; do you want to add it?}}
  associatedtype U // expected-note {{protocol requires nested type 'U'; do you want to add it?}}
}

struct S : P { // expected-error {{type 'S' does not conform to protocol 'P'}}
  typealias T = ImplicitlyUnwrappedOptional<Int> // expected-error {{'ImplicitlyUnwrappedOptional' has been renamed to 'Optional'}}{{17-44=Optional}}
  typealias U = Optional<ImplicitlyUnwrappedOptional<Int>> // expected-error {{'ImplicitlyUnwrappedOptional' has been renamed to 'Optional'}}{{26-53=Optional}}

  typealias V = Int!  // expected-error {{'!' is not allowed here; perhaps '?' was intended?}}{{20-21=?}}
  typealias W = Int!? // expected-error {{'!' is not allowed here; perhaps '?' was intended?}}{{20-21=?}}

  var x: V
  var y: W
  var fn1: (Int!) -> Int // expected-error {{'!' is not allowed here; perhaps '?' was intended?}}{{16-17=?}}
  var fn2: (Int) -> Int! // expected-error {{'!' is not allowed here; perhaps '?' was intended?}}{{24-25=?}}

  subscript (
    index: ImplicitlyUnwrappedOptional<Int> // expected-error {{'ImplicitlyUnwrappedOptional' has been renamed to 'Optional'}}{{12-39=Optional}}
  )     -> ImplicitlyUnwrappedOptional<Int> { // expected-error {{'ImplicitlyUnwrappedOptional' has been renamed to 'Optional'}}{{12-39=Optional}}
    return index
  }

  subscript<T> (
    index: ImplicitlyUnwrappedOptional<T> // expected-error {{'ImplicitlyUnwrappedOptional' has been renamed to 'Optional'}}{{12-39=Optional}}
  )     -> ImplicitlyUnwrappedOptional<T> { // expected-error {{'ImplicitlyUnwrappedOptional' has been renamed to 'Optional'}}{{12-39=Optional}}
    return index
  }
}

func generic<T : P>(_: T) where T.T == ImplicitlyUnwrappedOptional<Int> { } // expected-error {{'ImplicitlyUnwrappedOptional' has been renamed to 'Optional'}}{{40-67=Optional}}
func genericOptIUO<T : P>(_: T) where T.U == Optional<ImplicitlyUnwrappedOptional<Int>> {} // expected-error {{'ImplicitlyUnwrappedOptional' has been renamed to 'Optional'}}{{55-82=Optional}}

func testClosure() -> Int {
  return {
    (i: ImplicitlyUnwrappedOptional<Int>) // expected-error {{'ImplicitlyUnwrappedOptional' has been renamed to 'Optional'}}{{9-36=Optional}}
     -> ImplicitlyUnwrappedOptional<Int> in // expected-error {{'ImplicitlyUnwrappedOptional' has been renamed to 'Optional'}}{{9-36=Optional}}
    return i
  }(1)!
}

_ = Array<Int!>() // expected-error {{'!' is not allowed here; perhaps '?' was intended?}}{{14-15=?}}
let _: Array<Int!> = [1] // expected-error {{'!' is not allowed here; perhaps '?' was intended?}}{{17-18=?}}
_ = [Int!]() // expected-error {{'!' is not allowed here; perhaps '?' was intended?}}{{9-10=?}}
let _: [Int!] = [1] // expected-error {{'!' is not allowed here; perhaps '?' was intended?}}{{12-13=?}}
_ = Optional<Int!>(nil) // expected-error {{'!' is not allowed here; perhaps '?' was intended?}}{{17-18=?}}
let _: Optional<Int!> = nil // expected-error {{'!' is not allowed here; perhaps '?' was intended?}}{{20-21=?}}
_ = Int!?(0) // expected-error {{'!' is not allowed here; perhaps '?' was intended?}}{{8-9=?}}
let _: Int!? = 0 // expected-error {{'!' is not allowed here; perhaps '?' was intended?}}{{11-12=?}}
_ = (
  Int!, // expected-error {{'!' is not allowed here; perhaps '?' was intended?}}{{6-7=?}}
  Float!, // expected-error {{'!' is not allowed here; perhaps '?' was intended?}}{{8-9=?}}
  String! // expected-error {{'!' is not allowed here; perhaps '?' was intended?}}{{9-10=?}}
)(1, 2.0, "3")
let _: (
  Int!, // expected-error {{'!' is not allowed here; perhaps '?' was intended?}}{{6-7=?}}
  Float!, // expected-error {{'!' is not allowed here; perhaps '?' was intended?}}{{8-9=?}}
  String! // expected-error {{'!' is not allowed here; perhaps '?' was intended?}}{{9-10=?}}
) = (1, 2.0, "3")

struct Generic<T, U, C> {
  init(_ t: T, _ u: U, _ c: C) {}
}
_ = Generic<Int!, // expected-error {{'!' is not allowed here; perhaps '?' was intended?}}{{16-17=?}}
            Float!, // expected-error {{'!' is not allowed here; perhaps '?' was intended?}}{{18-19=?}}
            String!>(1, 2.0, "3") // expected-error {{'!' is not allowed here; perhaps '?' was intended?}}{{19-20=?}}
let _: Generic<Int!, // expected-error {{'!' is not allowed here; perhaps '?' was intended?}}{{19-20=?}}
               Float!, // expected-error {{'!' is not allowed here; perhaps '?' was intended?}}{{21-22=?}}
               String!> = Generic(1, 2.0, "3") // expected-error {{'!' is not allowed here; perhaps '?' was intended?}}{{22-23=?}}

func vararg(_ first: Int, more: Int!...) { // expected-error {{'!' is not allowed here; perhaps '?' was intended?}}{{36-37=?}}
}

func varargIdentifier(_ first: Int, more: ImplicitlyUnwrappedOptional<Int>...) { // expected-error {{'ImplicitlyUnwrappedOptional' has been renamed to 'Optional'}}{{43-70=Optional}}
}

func iuoInTuple() -> (Int!) { // expected-error {{'!' is not allowed here; perhaps '?' was intended?}}{{26-27=?}}
  return 1
}

func iuoInTupleIdentifier() -> (ImplicitlyUnwrappedOptional<Int>) { // expected-error {{'ImplicitlyUnwrappedOptional' has been renamed to 'Optional'}}{{33-60=Optional}}
  return 1
}

func iuoInTuple2() -> (Float, Int!) { // expected-error {{'!' is not allowed here; perhaps '?' was intended?}}{{34-35=?}}
  return (1.0, 1)
}

func iuoInTuple2Identifier() -> (Float, ImplicitlyUnwrappedOptional<Int>) { // expected-error {{'ImplicitlyUnwrappedOptional' has been renamed to 'Optional'}}{{41-68=Optional}}
  return (1.0, 1)
}

func takesFunc(_ fn: (Int!) -> Int) -> Int { // expected-error {{'!' is not allowed here; perhaps '?' was intended?}}{{26-27=?}}
  return fn(0)
}

func takesFuncIdentifier(_ fn: (ImplicitlyUnwrappedOptional<Int>) -> Int) -> Int { // expected-error {{'ImplicitlyUnwrappedOptional' has been renamed to 'Optional'}}{{33-60=Optional}}
  return fn(0)
}

func takesFunc2(_ fn: (Int) -> Int!) -> Int { // expected-error {{'!' is not allowed here; perhaps '?' was intended?}}{{35-36=?}}
  return fn(0)!
}

func takesFunc2Identifier(_ fn: (Int) -> ImplicitlyUnwrappedOptional<Int>) -> Int { // expected-error {{'ImplicitlyUnwrappedOptional' has been renamed to 'Optional'}}{{42-69=Optional}}
  return fn(0)!
}

func returnsFunc() -> (Int!) -> Int { // expected-error {{'!' is not allowed here; perhaps '?' was intended?}}{{27-28=?}}
  return { $0! }
}

func returnsFuncIdentifier() -> (ImplicitlyUnwrappedOptional<Int>) -> Int { // expected-error {{'ImplicitlyUnwrappedOptional' has been renamed to 'Optional'}}{{34-61=Optional}}
  return { $0! }
}

func returnsFunc2() -> (Int) -> Int! { // expected-error {{'!' is not allowed here; perhaps '?' was intended?}}{{36-37=?}}
  return { $0 }
}

func returnsFunc2Identifier() -> (Int) -> ImplicitlyUnwrappedOptional<Int> { // expected-error {{'ImplicitlyUnwrappedOptional' has been renamed to 'Optional'}}{{43-70=Optional}}
  return { $0 }
}

let x0 = 1 as ImplicitlyUnwrappedOptional // expected-error {{'ImplicitlyUnwrappedOptional' has been renamed to 'Optional'}}{{15-42=Optional}}

let x: Int? = 1
let y0: Int = x as Int! // expected-error {{using '!' is not allowed here; perhaps '?' was intended?}}{{23-24=?}}
let y1: Int = (x as Int!)! // expected-error {{using '!' is not allowed here; perhaps '?' was intended?}}{{24-25=?}}
let z0: Int = x as! Int! // expected-error {{using '!' is not allowed here; perhaps '?' was intended?}}{{24-25=?}}
// expected-warning@-1 {{forced cast from 'Int?' to 'Int' only unwraps optionals; did you mean to use '!'?}}
let z1: Int = (x as! Int!)! // expected-error {{using '!' is not allowed here; perhaps '?' was intended?}}{{25-26=?}}
// expected-warning@-1 {{forced cast of 'Int?' to same type has no effect}}
let w0: Int = (x as? Int!)! // expected-warning {{conditional cast from 'Int?' to 'Int?' always succeeds}}
// expected-error@-1 {{using '!' is not allowed here; perhaps '?' was intended?}}{{25-26=?}}
let w1: Int = (x as? Int!)!! // expected-warning {{conditional cast from 'Int?' to 'Int?' always succeeds}}
// expected-error@-1 {{using '!' is not allowed here; perhaps '?' was intended?}}{{25-26=?}}

func overloadedByOptionality(_ a: inout Int!) {}
// expected-note@-1 {{'overloadedByOptionality' previously declared here}}
func overloadedByOptionality(_ a: inout Int?) {}
// expected-error@-1 {{invalid redeclaration of 'overloadedByOptionality'}}

struct T {
  let i: Int!
  var j: Int!
  let k: Int
}

func select(i: Int!, m: Int, t: T) {
  let _ = i ? i : m // expected-error {{result values in '? :' expression have mismatching types 'Int?' and 'Int'}}
  let _ = t.i ? t.j : t.k // expected-error {{result values in '? :' expression have mismatching types 'Int?' and 'Int'}}
}
