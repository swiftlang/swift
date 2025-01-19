// RUN: %target-typecheck-verify-swift -swift-version 5 -verify-additional-prefix swift5-
// RUN: %target-typecheck-verify-swift -swift-version 4 -verify-additional-prefix swift4-

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
  _: [Int!]
  // expected-swift4-warning@-1:10 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
  // expected-swift5-error@-2:10 {{'!' is not allowed here}}{{none}}
  // expected-note@-3:10 {{use '?' instead}}{{10-11=?}}
) -> [Int!] {
  // expected-swift4-warning@-1:10 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
  // expected-swift5-error@-2:10 {{'!' is not allowed here}}{{none}}
  // expected-note@-3:10 {{use '?' instead}}{{10-11=?}}
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

// FIXME: Duplicate diagnostics in -swift-version 4
func genericFunctionSigilArray<T>(
  iuo: [T!]
  // expected-swift4-warning@-1:10 2 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
  // expected-swift5-error@-2:10 {{'!' is not allowed here}}{{none}}
  // expected-swift4-note@-3:10 2 {{use '?' instead}}{{10-11=?}}
  // expected-swift5-note@-4:10 {{use '?' instead}}{{10-11=?}}
) -> [T!] {
  // expected-swift4-warning@-1:8 2 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
  // expected-swift5-error@-2:8 {{'!' is not allowed here}}{{none}}
  // expected-swift4-note@-3:8 2 {{use '?' instead}}{{8-9=?}}
  // expected-swift5-note@-4:8 {{use '?' instead}}{{8-9=?}}
  return iuo
}

protocol P {
  associatedtype T
  associatedtype U
}

struct S : P {
  typealias T = ImplicitlyUnwrappedOptional<Int> // expected-error {{'ImplicitlyUnwrappedOptional' has been renamed to 'Optional'}}{{17-44=Optional}}
  typealias U = Optional<ImplicitlyUnwrappedOptional<Int>> // expected-error {{'ImplicitlyUnwrappedOptional' has been renamed to 'Optional'}}{{26-53=Optional}}

  typealias V = Int!
  // expected-swift4-warning@-1:20 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
  // expected-swift5-error@-2:20 {{'!' is not allowed here}}{{none}}
  // expected-note@-3:20 {{use '?' instead}}{{20-21=?}}
  typealias W = Int!?
  // expected-swift4-warning@-1:20 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
  // expected-swift5-error@-2:20 {{'!' is not allowed here}}{{none}}
  // expected-note@-3:20 {{use '?' instead}}{{20-21=?}}

  var x: V
  var y: W
  var fn1: (Int!) -> Int
  // expected-swift4-warning@-1:16 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
  // expected-swift5-error@-2:16 {{'!' is not allowed here}}{{none}}
  // expected-note@-3:16 {{use '?' instead}}{{16-17=?}}
  var fn2: (Int) -> Int!
  // expected-swift4-warning@-1:24 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
  // expected-swift5-error@-2:24 {{'!' is not allowed here}}{{none}}
  // expected-note@-3:24 {{use '?' instead}}{{24-25=?}}

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

_ = Array<Int!>()
// expected-swift4-warning@-1:14 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
// expected-swift5-error@-2:14 {{'!' is not allowed here}}{{none}}
// expected-note@-3:14 {{use '?' instead}}{{14-15=?}}
let _: Array<Int!> = [1]
// expected-swift4-warning@-1:17 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
// expected-swift5-error@-2:17 {{'!' is not allowed here}}{{none}}
// expected-note@-3:17 {{use '?' instead}}{{17-18=?}}
_ = [Int!]()
// expected-swift4-warning@-1:9 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
// expected-swift5-error@-2:9 {{'!' is not allowed here}}{{none}}
// expected-note@-3:9 {{use '?' instead}}{{9-10=?}}
// <rdar://problem/21684837> typeexpr not being formed for postfix !
let _ = [Int!](repeating: nil, count: 2)
// expected-swift4-warning@-1:13 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
// expected-swift5-error@-2:13 {{'!' is not allowed here}}{{none}}
// expected-note@-3:13 {{use '?' instead}}{{13-14=?}}
let _: [Int!] = [1]
// expected-swift4-warning@-1:12 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
// expected-swift5-error@-2:12 {{'!' is not allowed here}}{{none}}
// expected-note@-3:12 {{use '?' instead}}{{12-13=?}}

// FIXME: Duplicate diagnostics in -swift-version 4
_ = Optional<Int!>(nil)
// expected-swift4-warning@-1:17 2 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
// expected-swift5-error@-2:17 {{'!' is not allowed here}}{{none}}
// expected-swift4-note@-3:17 2 {{use '?' instead}}{{17-18=?}}
// expected-swift5-note@-4:17 {{use '?' instead}}{{17-18=?}}
let _: Optional<Int!> = nil
// expected-swift4-warning@-1:20 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
// expected-swift5-error@-2:20 {{'!' is not allowed here}}{{none}}
// expected-note@-3:20 {{use '?' instead}}{{20-21=?}}

// FIXME: Duplicate diagnostics in -swift-version 4
_ = Int!?(0)
// expected-swift4-warning@-1:8 2 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
// expected-swift5-error@-2:8 {{'!' is not allowed here}}{{none}}
// expected-swift4-note@-3:8 2 {{use '?' instead}}{{8-9=?}}
// expected-swift5-note@-4:8 {{use '?' instead}}{{8-9=?}}
let _: Int!? = 0
// expected-swift4-warning@-1:11 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
// expected-swift5-error@-2:11 {{'!' is not allowed here}}{{none}}
// expected-note@-3:11 {{use '?' instead}}{{11-12=?}}
_ = (
  Int!,
  // expected-swift4-warning@-1:6 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
  // expected-swift5-error@-2:6 {{'!' is not allowed here}}{{none}}
  // expected-note@-3:6 {{use '?' instead}}{{6-7=?}}
  Float!,
  // expected-swift4-warning@-1:8 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
  // expected-swift5-error@-2:8 {{'!' is not allowed here}}{{none}}
  // expected-note@-3:8 {{use '?' instead}}{{8-9=?}}
  String!
  // expected-swift4-warning@-1:9 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
  // expected-swift5-error@-2:9 {{'!' is not allowed here}}{{none}}
  // expected-note@-3:9 {{use '?' instead}}{{9-10=?}}
)(1, 2.0, "3")
let _: (
  Int!,
  // expected-swift4-warning@-1:6 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
  // expected-swift5-error@-2:6 {{'!' is not allowed here}}{{none}}
  // expected-note@-3:6 {{use '?' instead}}{{6-7=?}}
  Float!,
  // expected-swift4-warning@-1:8 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
  // expected-swift5-error@-2:8 {{'!' is not allowed here}}{{none}}
  // expected-note@-3:8 {{use '?' instead}}{{8-9=?}}
  String!
  // expected-swift4-warning@-1:9 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
  // expected-swift5-error@-2:9 {{'!' is not allowed here}}{{none}}
  // expected-note@-3:9 {{use '?' instead}}{{9-10=?}}
) = (1, 2.0, "3")

struct Generic<T, U, C> {
  init(_ t: T, _ u: U, _ c: C) {}
}
_ = Generic<Int!,
// expected-swift4-warning@-1:16 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
// expected-swift5-error@-2:16 {{'!' is not allowed here}}{{none}}
// expected-note@-3:16 {{use '?' instead}}{{16-17=?}}
            Float!,
// expected-swift4-warning@-1:18 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
// expected-swift5-error@-2:18 {{'!' is not allowed here}}{{none}}
// expected-note@-3:18 {{use '?' instead}}{{18-19=?}}
            String!>(1, 2.0, "3")
// expected-swift4-warning@-1:19 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
// expected-swift5-error@-2:19 {{'!' is not allowed here}}{{none}}
// expected-note@-3:19 {{use '?' instead}}{{19-20=?}}
let _: Generic<Int!,
// expected-swift4-warning@-1:19 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
// expected-swift5-error@-2:19 {{'!' is not allowed here}}{{none}}
// expected-note@-3:19 {{use '?' instead}}{{19-20=?}}
               Float!,
// expected-swift4-warning@-1:21 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
// expected-swift5-error@-2:21 {{'!' is not allowed here}}{{none}}
// expected-note@-3:21 {{use '?' instead}}{{21-22=?}}
               String!> = Generic(1, 2.0, "3")
// expected-swift4-warning@-1:22 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
// expected-swift5-error@-2:22 {{'!' is not allowed here}}{{none}}
// expected-note@-3:22 {{use '?' instead}}{{22-23=?}}

extension Optional {
  typealias Wrapped = Wrapped
}
let _: Int!.Wrapped
// expected-swift4-warning@-1:11 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
// expected-swift5-error@-2:11 {{'!' is not allowed here}}{{none}}
// expected-note@-3:11 {{use '?' instead}}{{11-12=?}}
let _: (Int!).Wrapped
// expected-swift4-warning@-1:12 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
// expected-swift5-error@-2:12 {{'!' is not allowed here}}{{none}}
// expected-note@-3:12 {{use '?' instead}}{{12-13=?}}

func vararg(_ first: Int, more: Int!...) {
// expected-swift4-warning@-1:36 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
// expected-swift5-error@-2:36 {{'!' is not allowed here}}{{none}}
// expected-note@-3:36 {{use '?' instead}}{{36-37=?}}
}

func varargIdentifier(_ first: Int, more: ImplicitlyUnwrappedOptional<Int>...) { // expected-error {{'ImplicitlyUnwrappedOptional' has been renamed to 'Optional'}}{{43-70=Optional}}
}

func iuoInTuple() -> (Int!) {
// expected-swift4-warning@-1:26 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
// expected-swift5-error@-2:26 {{'!' is not allowed here}}{{none}}
// expected-note@-3:26 {{use '?' instead}}{{26-27=?}}
  return 1
}

func iuoInTupleIdentifier() -> (ImplicitlyUnwrappedOptional<Int>) { // expected-error {{'ImplicitlyUnwrappedOptional' has been renamed to 'Optional'}}{{33-60=Optional}}
  return 1
}

func iuoInTuple2() -> (Float, Int!) {
// expected-swift4-warning@-1:34 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
// expected-swift5-error@-2:34 {{'!' is not allowed here}}{{none}}
// expected-note@-3:34 {{use '?' instead}}{{34-35=?}}
  return (1.0, 1)
}

func iuoInTuple2Identifier() -> (Float, ImplicitlyUnwrappedOptional<Int>) { // expected-error {{'ImplicitlyUnwrappedOptional' has been renamed to 'Optional'}}{{41-68=Optional}}
  return (1.0, 1)
}

func takesFunc(_ fn: (Int!) -> Int) -> Int {
// expected-swift4-warning@-1:26 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
// expected-swift5-error@-2:26 {{'!' is not allowed here}}{{none}}
// expected-note@-3:26 {{use '?' instead}}{{26-27=?}}
  return fn(0)
}

func takesFuncIdentifier(_ fn: (ImplicitlyUnwrappedOptional<Int>) -> Int) -> Int { // expected-error {{'ImplicitlyUnwrappedOptional' has been renamed to 'Optional'}}{{33-60=Optional}}
  return fn(0)
}

func takesFunc2(_ fn: (Int) -> Int!) -> Int {
// expected-swift4-warning@-1:35 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
// expected-swift5-error@-2:35 {{'!' is not allowed here}}{{none}}
// expected-note@-3:35 {{use '?' instead}}{{35-36=?}}
  return fn(0)!
}

func takesFunc2Identifier(_ fn: (Int) -> ImplicitlyUnwrappedOptional<Int>) -> Int { // expected-error {{'ImplicitlyUnwrappedOptional' has been renamed to 'Optional'}}{{42-69=Optional}}
  return fn(0)!
}

func returnsFunc() -> (Int!) -> Int {
// expected-swift4-warning@-1:27 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
// expected-swift5-error@-2:27 {{'!' is not allowed here}}{{none}}
// expected-note@-3:27 {{use '?' instead}}{{27-28=?}}
  return { $0! }
}

func returnsFuncIdentifier() -> (ImplicitlyUnwrappedOptional<Int>) -> Int { // expected-error {{'ImplicitlyUnwrappedOptional' has been renamed to 'Optional'}}{{34-61=Optional}}
  return { $0! }
}

func returnsFunc2() -> (Int) -> Int! {
// expected-swift4-warning@-1:36 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
// expected-swift5-error@-2:36 {{'!' is not allowed here}}{{none}}
// expected-note@-3:36 {{use '?' instead}}{{36-37=?}}
  return { $0 }
}

func returnsFunc2Identifier() -> (Int) -> ImplicitlyUnwrappedOptional<Int> { // expected-error {{'ImplicitlyUnwrappedOptional' has been renamed to 'Optional'}}{{43-70=Optional}}
  return { $0 }
}

func opaqueResult() -> (some Equatable)! { return 1 }
// expected-swift4-warning@-1:40 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
// expected-swift5-error@-2:40 {{'!' is not allowed here}}{{none}}
// expected-note@-3:40 {{use '?' instead}}{{40-41=?}}

let x0 = 1 as ImplicitlyUnwrappedOptional // expected-error {{'ImplicitlyUnwrappedOptional' has been renamed to 'Optional'}}{{15-42=Optional}}

let x: Int? = 1
let y0: Int = x as Int!
// expected-swift4-warning@-1:23 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
// expected-swift5-error@-2:23 {{'!' is not allowed here}}{{none}}
let y1: Int = (x as Int!)!
// expected-swift4-warning@-1:24 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
// expected-swift5-error@-2:24 {{'!' is not allowed here}}{{none}}
let z0: Int = x as! Int!
// expected-swift4-warning@-1:24 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
// expected-swift5-error@-2:24 {{'!' is not allowed here}}{{none}}
// expected-warning@-3 {{forced cast from 'Int?' to 'Int' only unwraps optionals; did you mean to use '!'?}}
let z1: Int = (x as! Int!)!
// expected-swift4-warning@-1:25 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
// expected-swift5-error@-2:25 {{'!' is not allowed here}}{{none}}
// expected-warning@-3 {{forced cast from 'Int?' to 'Int' only unwraps optionals; did you mean to use '!'?}}
let w0: Int = (x as? Int!)!
// expected-swift4-warning@-1:25 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
// expected-swift5-error@-2:25 {{'!' is not allowed here}}{{none}}
// expected-warning@-3 {{conditional cast from 'Int?' to 'Int?' always succeeds}}
let w1: Int = (x as? Int!)!!
// expected-swift4-warning@-1:25 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
// expected-swift5-error@-2:25 {{'!' is not allowed here}}{{none}}
// expected-warning@-3 {{conditional cast from 'Int?' to 'Int?' always succeeds}}

func overloadedByOptionality(_ a: inout Int!) {}
// expected-note@-1 {{'overloadedByOptionality' previously declared here}}
func overloadedByOptionality(_ a: inout Int?) {}
// expected-error@-1 {{invalid redeclaration of 'overloadedByOptionality'}}
// expected-note@-2 {{optional parameter is of same type as implicitly unwrapped optional parameter}}

struct T {
  let i: Int!
  var j: Int!
  let k: Int
}

func select(i: Int!, m: Int, t: T) {
  let _ = i ? i : m // expected-error {{optional type 'Int?' cannot be used as a boolean; test for '!= nil' instead}} {{11-11=(}} {{12-12= != nil)}}
  let _ = t.i ? t.j : t.k // expected-error {{optional type 'Int?' cannot be used as a boolean; test for '!= nil' instead}} {{11-11=(}} {{14-14= != nil)}}
}

class B {}
class D : B {
  var i: Int!
}

func coerceToIUO(d: D?) -> B {
  return d as B!
// expected-swift4-warning@-1:16 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
// expected-swift5-error@-2:16 {{using '!' is not allowed here}}
}

func forcedDowncastToOptional(b: B?) -> D? {
  return b as! D!
// expected-swift4-warning@-1:17 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
// expected-swift5-error@-2:17 {{using '!' is not allowed here}}
}

func forcedDowncastToObject(b: B?) -> D {
  return b as! D!
// expected-swift4-warning@-1:17 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
// expected-swift5-error@-2:17 {{using '!' is not allowed here}}
}

func forcedDowncastToObjectIUOMember(b: B?) -> Int {
  return (b as! D!).i
// expected-swift4-warning@-1:18 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
// expected-swift5-error@-2:18 {{using '!' is not allowed here}}
}

func forcedUnwrapViaForcedCast(b: B?) -> B {
  return b as! B! // expected-warning {{forced cast from 'B?' to 'B' only unwraps optionals; did you mean to use '!'?}}
// expected-swift4-warning@-1:17 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
// expected-swift5-error@-2:17 {{using '!' is not allowed here}}
}

func conditionalDowncastToOptional(b: B?) -> D? {
  return b as? D!
// expected-swift4-warning@-1:17 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
// expected-swift5-error@-2:17 {{using '!' is not allowed here}}
}

func conditionalDowncastToObject(b: B?) -> D {
  return b as? D! // expected-error {{value of optional type 'D?' must be unwrapped to a value of type 'D'}}
  // expected-note@-1 {{coalesce using '??' to provide a default when the optional value contains 'nil'}}
  // expected-note@-2 {{force-unwrap using '!' to abort execution if the optional value contains 'nil'}}
  // expected-swift4-warning@-3:17 {{using '!' here is deprecated; this is an error in the Swift 5 language mode}}{{none}}
  // expected-swift5-error@-4:17 {{using '!' is not allowed here}}
}
