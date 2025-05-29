// RUN: %target-typecheck-verify-swift -target %target-swift-5.7-abi-triple -dump-ast | %FileCheck %s

/// Used to verify the type of an expression. Use like this:
/// ```
/// var types = SwiftTypePair(typeOf: expr, type2: SwiftType<Int>.self)
/// types.assertTypesAreEqual()
/// ```
struct SwiftType<T> {}
struct SwiftTypePair<T1, T2> {
  init(typeOf: T1, type2: SwiftType<T2>.Type) {}

  mutating func assertTypesAreEqual() where T1 == T2 {}
}

protocol Q { }

protocol P {
  associatedtype A: Q
}

protocol P1<A> {
  associatedtype A
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
  // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
  let pa = acceptGeneric(p)
  let _: Int = pa // expected-error{{cannot convert value of type '(any Q)?' to specified type 'Int'}}

  var vp = p
  // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
  let vpa = acceptGeneric(vp)
  let _: Int = vpa // expected-error{{cannot convert value of type '(any Q)?' to specified type 'Int'}}

  // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
  let pqa = acceptGeneric(pq)
  let _: Int = pqa  // expected-error{{cannot convert value of type '(any Q)?' to specified type 'Int'}}

  // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
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
  // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
  let e = takeCollectionOfPs(cp)
  let _: Int = e // expected-error{{cannot convert value of type '(any Q)?' to specified type 'Int'}}
}

// --- Multiple opened existentials in the same expression
func takeTwoGenerics<T1: P, T2: P>(_ a: T1, _ b: T2) -> (T1, T2) { (a, b) }

extension P {
  func combineThePs<T: P & Q>(_ other: T) -> (A, T.A)? { nil }
}

func testMultipleOpened(a: any P, b: any P & Q) {
  // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
  let r1 = takeTwoGenerics(a, b)
  let _: Int = r1  // expected-error{{cannot convert value of type '(any P, any P & Q)' to specified type 'Int'}}

  // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
  let r2 = a.combineThePs(b)
  let _: Int = r2  // expected-error{{cannot convert value of type '(any Q, any Q)?' to specified type 'Int'}}
}

// --- Opening existential metatypes
func conjureValue<T: P>(of type: T.Type) -> T? {
  nil
}

func testMagic(pt: any P.Type) {
  // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
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
  // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
  takesInOut(&p)
}

func takesOptional<T: P>(_ value: T?) { }
// expected-note@-1{{required by global function 'takesOptional' where 'T' = 'any P'}}

func passesToOptional(p: any P, pOpt: (any P)?) {
  // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
  takesOptional(p) // okay
  takesOptional(pOpt) // expected-error{{type 'any P' cannot conform to 'P'}}
  // expected-note@-1{{only concrete types such as structs, enums and classes can conform to protocols}}
}


@available(SwiftStdlib 5.1, *)
func testReturningOpaqueTypes(p: any P) {
  // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
  let q = p.getQ()
  let _: Int = q  // expected-error{{cannot convert value of type 'any Q' to specified type 'Int'}}

  p.getCollectionOf() // expected-error{{member 'getCollectionOf' cannot be used on value of type 'any P'; consider using a generic constraint instead}}

  // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
  let q2 = getPQ(p)
  let _: Int = q2  // expected-error{{cannot convert value of type 'any Q' to specified type 'Int'}}

  getCollectionOfP(p) // expected-error{{type 'any P' cannot conform to 'P'}}
  // expected-note@-1{{only concrete types such as structs, enums and classes can conform to protocols}}

  // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
  let fi = funnyIdentity(p)
  let _: Int = fi // expected-error{{cannot convert value of type '(any P)?' to specified type 'Int'}}

  // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
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
  // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
  takeValueAndClosure(p) { x in
    var types = SwiftTypePair(typeOf: x, type2: SwiftType<any P>.self)
    types.assertTypesAreEqual()

    return ()
  }

  // Do not erase when referring to a generic function.
  // FIXME:
  // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
  takeValueAndClosure(p, body: genericFunctionTakingP)
  // FIXME:
  // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
  takeValueAndClosure(p, body: overloadedGenericFunctionTakingP)
  takeValueAndClosure(p, body: genericFunctionTakingPQ) // expected-error{{global function 'genericFunctionTakingPQ' requires that 'T' conform to 'Q'}}

  // Do not allow opening if there are any uses of the type parameter before
  // the opened parameter. This maintains left-to-right evaluation order.
  takeValueAndClosureBackwards( // expected-error{{type 'any P' cannot conform to 'P'}}
    // expected-note@-1{{only concrete types such as structs, enums and classes can conform to protocols}}
    body: { x in x as Int }, // expected-error{{'any P' is not convertible to 'Int'}}
    // expected-note@-1{{did you mean to use 'as!' to force downcast?}}
    p)
}

protocol B {
  associatedtype C: P where C.A == Double
  associatedtype D: P
  associatedtype E: P1 where E.A == Double
}

protocol D {
  associatedtype E
}

extension B {
  var testVar: (Int, [C]) { get { fatalError() } }

  func getC() -> C { fatalError() }
}

func testExplicitCoercionRequirement(v: any B, otherV: any B & D) {
  func getC<T: B>(_: T) -> T.C { fatalError() }
  func getE<T: B>(_: T) -> T.E { fatalError() }
  func getTuple<T: B>(_: T) -> (T, T.C) { fatalError() }
  func getNoError<T: B>(_: T) -> T.C.A { fatalError() }
  func getComplex<T: B>(_: T) -> ([(x: (a: T.C, b: Int), y: Int)], [Int: T.C]) { fatalError() }

  func overloaded<T: B>(_: T) -> (x: Int, y: T.C) { fatalError() }
  func overloaded<T: P>(_: T) -> Int { 42 }

  // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
  _ = getC(v) // Ok
  // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
  _ = getC(v) as any P // Ok

  // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
  _ = getE(v) // Ok
  // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
  _ = getE(v) as any P1<Double> // Ok

  // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
  _ = getTuple(v) // Ok
  // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
  _ = getTuple(v) as (any B, any P) // Ok
  // Ok because T.C.A == Double
  // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
  _ = getNoError(v)

  // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
  _ = getComplex(v) // Ok
  // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
  _ = getComplex(v) as ([(x: (a: any P, b: Int), y: Int)], [Int : any P]) // Ok

  // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
  _ = overloaded(v) // Ok

  func acceptsAny<T>(_: T) {}

  // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
  acceptsAny(getC(v)) // Ok
  // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
  acceptsAny(getC(v) as any P) // Ok

  // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
  acceptsAny(getComplex(v)) // Ok
  // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
  acceptsAny(getComplex(v) as ([(x: (a: any P, b: Int), y: Int)], [Int : any P]))

  func getAssocNoRequirements<T: B>(_: T) -> (Int, [T.D]) { fatalError() }

  // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
  _ = getAssocNoRequirements(v) // Ok, `D` doesn't have any requirements

  // Test existential opening from protocol extension access
  _ = v.getC() // Ok
  _ = v.getC() as any P // Ok

  _ = v.testVar // Ok
  _ = v.testVar as (Int, [any P])

  func getF<T: D>(_: T) -> T.E { fatalError() }

  // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
  _ = getF(otherV) // Ok `E` doesn't have a `where` clause

  func getSelf<T: B>(_: T) -> T { fatalError() } // expected-note {{found this candidate}}
  func getSelf<T: D>(_: T) -> T { fatalError() } // expected-note {{found this candidate}}

  // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
  _ = getSelf(v) // Ok
  // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
  _ = getSelf(v) as any B // Ok
  _ = getSelf(otherV) as any B & D // expected-error {{ambiguous use of 'getSelf'}}

  func getBDSelf<T: D>(_: T) -> T { fatalError() }

  // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
  _ = getBDSelf(otherV) // Ok
  // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
  _ = getBDSelf(otherV) as any B & D // Ok

  func getP<T: P>(_: T) {}

  // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
  getP(getC(v)) // Ok
  // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
  getP(v.getC()) // Ok

  // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
  getP((getC(v) as any P))   // Ok - parens avoid opening suppression
  // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
  getP((v.getC() as any P))  // Ok - parens avoid opening suppression
}

class C1 {}
class C2<T>: C1 {}

// Test Associated Types
protocol P2 {
  associatedtype A
  associatedtype B: C2<A>

  func returnAssocTypeB() -> B
}

func testAssocReturn(p: any P2) {
  let _ = p.returnAssocTypeB()  // returns C1
}

protocol Q2 : P2 where A == Int {}

do {
  let q: any Q2
  let _ = q.returnAssocTypeB() // returns C1
}

// Test Primary Associated Types
protocol P3<A> {
  associatedtype A
  associatedtype B: C2<A>

  func returnAssocTypeB() -> B
}

func testAssocReturn(p: any P3<Int>) {
  let _ = p.returnAssocTypeB() // returns C2<A>
}

func testAssocReturn(p: any P3<any P3<String>>) {
  let _ = p.returnAssocTypeB()
}

protocol P4<A> {
  associatedtype A
  associatedtype B: C2<A>

  func returnPrimaryAssocTypeA() -> A
  func returnAssocTypeCollection() -> any Collection<A>
}

 //Confirm there is no way to access Primary Associated Type directly
func testPrimaryAssocReturn(p: any P4<Int>) {
  let _ = p.returnPrimaryAssocTypeA()
}

func testPrimaryAssocCollection(p: any P4<Float>) {
  let _: any Collection<Float> = p.returnAssocTypeCollection()
}

protocol P5<X> {
  associatedtype X = Void
}

struct K<T>: P5 {
  typealias X = T
}

extension P5 {
  @available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
  func foo() -> some P5<X>{
    K<X>()
  }
  func bar(_ handler: @escaping (X) -> Void) -> some P5<X> {
    K<X>()
  }
}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
func testFoo(_ p: any P5<String>) -> any P5 {
  p.foo()
}

func testFooGeneric<U>(_ p: any P5<Result<U, Error>>) -> any P5 {
  p.foo()
}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
func testBar<U>(_ p: any P5<Result<U, Error>>) -> any P5 {
  p.bar { _ in }
}

enum Node<T> {
  case e(any P5)
  case f(any P5<Result<T, Error>>)
}

struct S<T, U> {
  @available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
  func foo(_ elt: Node<U>) -> Node<T>? {
    switch elt {
    case let .e(p):
      return .e(p)
    case let .f(p):
      return .e(p.bar { _ in })
    }
  }
}

do {
  func nestedMetatypeCallee<T>(_ t: T) {}

  let t = String.Type.Type.Type.self as (any Q.Type.Type.Type.Type)
  // CHECK-NOT: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
  nestedMetatypeCallee(t)
}

do {
  class C<T> {}
  protocol P {}

  func f<T: P>(_: T, _: (() -> any (P & C<T>).Type)? = nil) {}
  // expected-note@-1 {{required by local function 'f' where 'T' = 'any P'}}

  let p: any P
  // CHECK-NOT: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
  f(p)
  // expected-error@-1 {{type 'any P' cannot conform to 'P'}}
  // expected-note@-2 {{only concrete types such as structs, enums and classes can conform to protocols}}
}

do {
  protocol P {}

  func foo<T: P>(_ m: inout T.Type) {}

  // expected-note@+1 {{change 'let' to 'var' to make it mutable}}
  let rValueP: P.Type
  var lValueP: P.Type

  // expected-error@+1 {{cannot pass immutable value as inout argument: 'rValueP' is a 'let' constant}}
  foo(&rValueP)
  // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
  foo(&lValueP)
}

do {
  do {
    func foo<T : BitwiseCopyable>(_: T) -> T {}

    let exist: any Any.Type
    // CHECK: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
    let result = foo(exist)
    do {
      // FIXME: The result type should be 'any Any.Type'
      // var types = SwiftTypePair(typeOf: result, type2: SwiftType<any Any.Type>.self)
      var types = SwiftTypePair(typeOf: result, type2: SwiftType<Any>.self)
      types.assertTypesAreEqual()
    }
  }

  do {
    func foo<T : BitwiseCopyable>(_: T) -> T {}

    let exist: any Any.Type.Type
    // CHECK-NOT: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
    let result = foo(exist)
    do {
      var types = SwiftTypePair(typeOf: result, type2: SwiftType<any Any.Type.Type>.self)
      types.assertTypesAreEqual()
    }
  }

  do {
    func foo<T>(_: T) -> T {}

    let exist: any Any.Type
    // CHECK-NOT: open_existential_expr {{.*}} location={{.*}}:[[@LINE+1]]:{{[0-9]+}} range=
    let result = foo(exist)
    do {
      var types = SwiftTypePair(typeOf: result, type2: SwiftType<any Any.Type>.self)
      types.assertTypesAreEqual()
    }
  }
}

// rdar://91922018
do {
  func f<E>(_ c: some Collection<E>) -> some Collection<E> {
    return c
  }
  let c: any Collection<Int>
  let result = f(c)
  do {
    var types = SwiftTypePair(typeOf: result, type2: SwiftType<any Collection<Int>>.self)
    types.assertTypesAreEqual()
  }
}

struct G<A>: PP3 {}

protocol PP1 {
    associatedtype A
}

extension PP1 {
    func f(p: any PP2<G<Self.A>>) {
        p.g(t: self)
    }
}

protocol PP2<B> {
    associatedtype A
    associatedtype B: PP3 where Self.B.A == Self.A
}

extension PP2 {
    func g<T: PP1>(t: T) where Self.B == G<T.A> {}
}

protocol PP3 {
    associatedtype A
}

