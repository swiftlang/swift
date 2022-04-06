// RUN: %target-typecheck-verify-swift -enable-experimental-opened-existential-types

protocol Q { }

protocol P {
  associatedtype A: Q
}

extension Int: P {
  typealias A = Double
}

extension Array: P where Element: P {
  typealias A = String
}

extension Double: Q { }
extension String: Q { }

func acceptGeneric<T: P>(_: T) -> T.A? { nil }
func acceptCollection<C: Collection>(_ c: C) -> C.Element { c.first! }

// --- Simple opening of existential values
func testSimpleExistentialOpening(p: any P, pq: any P & Q, c: any Collection) {
  let pa = acceptGeneric(p)
  let _: Int = pa // expected-error{{cannot convert value of type '(any Q)?' to specified type 'Int'}}

  let pqa = acceptGeneric(pq)
  let _: Int = pqa  // expected-error{{cannot convert value of type '(any Q)?' to specified type 'Int'}}

  let element = acceptCollection(c) 
  let _: Int = element // expected-error{{cannot convert value of type 'Any' to specified type 'Int'}}
}

// --- Requirements on nested types
protocol CollectionOfPs: Collection where Self.Element: P { }

func takeCollectionOfPs<C: Collection>(_: C) -> C.Element.A?    
  where C.Element: P
{
  nil
}

func testCollectionOfPs(cp: any CollectionOfPs) {
  let e = takeCollectionOfPs(cp)
  let _: Int = e // expected-error{{cannot convert value of type '(any Q)?' to specified type 'Int'}}
}

// --- Multiple opened existentials in the same expression
func takeTwoGenerics<T1: P, T2: P>(_ a: T1, _ b: T2) -> (T1, T2) { (a, b) }

extension P {
  func combineThePs<T: P & Q>(_ other: T) -> (A, T.A)? { nil }
}

func testMultipleOpened(a: any P, b: any P & Q) {
  let r1 = takeTwoGenerics(a, b)
  let _: Int = r1  // expected-error{{cannot convert value of type '(any P, any P & Q)' to specified type 'Int'}}

  let r2 = a.combineThePs(b)
  let _: Int = r2  // expected-error{{cannot convert value of type '(any Q, any Q)?' to specified type 'Int'}}
}

// --- Opening existential metatypes
func conjureValue<T: P>(of type: T.Type) -> T? {
  nil
}

func testMagic(pt: any P.Type) {
  let pOpt = conjureValue(of: pt)
  let _: Int = pOpt // expected-error{{cannot convert value of type '(any P)?' to specified type 'Int'}}
}

// --- With primary associated types and opaque parameter types
protocol CollectionOf<Element>: Collection { }

extension Array: CollectionOf { }
extension Set: CollectionOf { }

// expected-note@+2{{required by global function 'reverseIt' where 'some CollectionOf<T>' = 'any CollectionOf'}}
@available(SwiftStdlib 5.1, *)
func reverseIt<T>(_ c: some CollectionOf<T>) -> some CollectionOf<T> {
  return c.reversed()
}

@available(SwiftStdlib 5.1, *)
func useReverseIt(_ c: any CollectionOf) {
  // Can't type-erase the `T` from the result.
  _ = reverseIt(c) // expected-error{{type 'any CollectionOf' cannot conform to 'CollectionOf'}}
  // expected-note@-1{{only concrete types such as structs, enums and classes can conform to protocols}}
}

/// --- Opening existentials when returning opaque types.
@available(SwiftStdlib 5.1, *)
extension P {
  func getQ() -> some Q {
    let a: A? = nil
    return a!
  }

  func getCollectionOf() -> some CollectionOf<A> {
    return [] as [A]
  }
}

@available(SwiftStdlib 5.1, *)
func getPQ<T: P>(_: T) -> some Q {
  let a: T.A? = nil
  return a!
}

// expected-note@+2{{required by global function 'getCollectionOfP' where 'T' = 'any P'}}
@available(SwiftStdlib 5.1, *)
func getCollectionOfP<T: P>(_: T) -> some CollectionOf<T.A> {
  return [] as [T.A]
}

func funnyIdentity<T: P>(_ value: T) -> T? {
  value
}

func arrayOfOne<T: P>(_ value: T) -> [T] {
  [value]
}

struct X<T: P> {
  // expected-note@-1{{required by generic struct 'X' where 'T' = 'any P'}}
  func f(_: T) { }
}

// expected-note@+1{{required by global function 'createX' where 'T' = 'any P'}}
func createX<T: P>(_ value: T) -> X<T> {
  X<T>()
}

func doNotOpenOuter(p: any P) {
  _ = X().f(p) // expected-error{{type 'any P' cannot conform to 'P'}}
  // expected-note@-1{{only concrete types such as structs, enums and classes can conform to protocols}}
}

func takesVariadic<T: P>(_ args: T...) { }
// expected-note@-1 2{{required by global function 'takesVariadic' where 'T' = 'any P'}}
// expected-note@-2{{in call to function 'takesVariadic'}}

func callVariadic(p1: any P, p2: any P) {
  takesVariadic() // expected-error{{generic parameter 'T' could not be inferred}}
  takesVariadic(p1) // expected-error{{type 'any P' cannot conform to 'P'}}
  // expected-note@-1{{only concrete types such as structs, enums and classes can conform to protocols}}
  takesVariadic(p1, p2) // expected-error{{type 'any P' cannot conform to 'P'}}
  // expected-note@-1{{only concrete types such as structs, enums and classes can conform to protocols}}
}

func takesInOut<T: P>(_ value: inout T) { }

func passesInOut(i: Int) {
  var p: any P = i
  takesInOut(&p)
}

@available(SwiftStdlib 5.1, *)
func testReturningOpaqueTypes(p: any P) {
  let q = p.getQ()
  let _: Int = q  // expected-error{{cannot convert value of type 'any Q' to specified type 'Int'}}

  p.getCollectionOf() // expected-error{{member 'getCollectionOf' cannot be used on value of type 'any P'; consider using a generic constraint instead}}

  let q2 = getPQ(p)
  let _: Int = q2  // expected-error{{cannot convert value of type 'any Q' to specified type 'Int'}}

  getCollectionOfP(p) // expected-error{{type 'any P' cannot conform to 'P'}}
  // expected-note@-1{{only concrete types such as structs, enums and classes can conform to protocols}}

  let fi = funnyIdentity(p)
  let _: Int = fi // expected-error{{cannot convert value of type '(any P)?' to specified type 'Int'}}

  _ = arrayOfOne(p) // okay, arrays are covariant in their argument

  _ = createX(p) // expected-error{{type 'any P' cannot conform to 'P'}}
  // expected-note@-1{{only concrete types such as structs, enums and classes can conform to protocols}}
}

// Type-erasing vs. opening for parameters after the opened one.
func takeValueAndClosure<T: P>(_ value: T, body: (T) -> Void) { }
func takeValueAndClosureBackwards<T: P>(body: (T) -> Void, _ value: T) { }
// expected-note@-1{{required by global function 'takeValueAndClosureBackwards(body:_:)' where 'T' = 'any P'}}

func genericFunctionTakingP<T: P>(_: T) { }
func genericFunctionTakingPQ<T: P & Q>(_: T) { }

func overloadedGenericFunctionTakingP<T: P>(_: T) -> Int { 0 }
func overloadedGenericFunctionTakingP<T: P>(_: T) { }

func testTakeValueAndClosure(p: any P) {
  // Type-erase when not provided with a generic function.
  takeValueAndClosure(p) { x in
    print(x)
    let _: Int = x // expected-error{{cannot convert value of type 'any P' to specified type 'Int'}}
  }

  // Do not erase when referring to a generic function.
  takeValueAndClosure(p, body: genericFunctionTakingP)
  takeValueAndClosure(p, body: overloadedGenericFunctionTakingP)
  takeValueAndClosure(p, body: genericFunctionTakingPQ) // expected-error{{global function 'genericFunctionTakingPQ' requires that 'T' conform to 'Q'}}

  // Do not allow opening if there are any uses of the the type parameter before
  // the opened parameter. This maintains left-to-right evaluation order.
  takeValueAndClosureBackwards( // expected-error{{type 'any P' cannot conform to 'P'}}
    // expected-note@-1{{only concrete types such as structs, enums and classes can conform to protocols}}
    body: { x in x as Int }, // expected-error{{'any P' is not convertible to 'Int'}}
    // expected-note@-1{{did you mean to use 'as!' to force downcast?}}
    p)
}
