// RUN: %target-swift-frontend -typecheck -parse-as-library %s -verify

struct Sub: Hashable {
  static func ==(_: Sub, _: Sub) -> Bool { return true }
  var hashValue: Int { return 0 }
}
struct OptSub: Hashable {
  static func ==(_: OptSub, _: OptSub) -> Bool { return true }
  var hashValue: Int { return 0 }
}
struct NonHashableSub {}

struct Prop {
  subscript(sub: Sub) -> A { get { return A() } set { } }
  subscript(optSub: OptSub) -> A? { get { return A() } set { } }
  subscript(nonHashableSub: NonHashableSub) -> A { get { return A() } set { } }
  subscript(a: Sub, b: Sub) -> A { get { return A() } set { } }
  subscript(a: Sub, b: NonHashableSub) -> A { get { return A() } set { } }

  var nonMutatingProperty: B {
    get { fatalError() }
    nonmutating set { fatalError() }
  }
}

struct A: Hashable {
  init() { fatalError() }

  var property: Prop
  var optProperty: Prop?
  let optLetProperty: Prop?

  subscript(sub: Sub) -> A { get { return self } set { } }

  static func ==(_: A, _: A) -> Bool { fatalError() }
  var hashValue: Int { fatalError() }
}
struct B {}
struct C<T> {
  var value: T
  subscript() -> T { get { return value } }
  subscript(sub: Sub) -> T { get { return value } set { } }
  subscript<U: Hashable>(sub: U) -> U { get { return sub } set { } }
  subscript<X>(noHashableConstraint sub: X) -> X { get { return sub } set { } }
}

extension Array where Element == A {
  var property: Prop { fatalError() }
}

protocol P { var member: String { get } }
extension B : P { var member : String { return "Member Value" } }

struct Exactly<T> {}

func expect<T>(_ x: inout T, toHaveType _: Exactly<T>.Type) {}

func testKeyPath(sub: Sub, optSub: OptSub,
                 nonHashableSub: NonHashableSub, x: Int) {
  var a = \A.property
  expect(&a, toHaveType: Exactly<WritableKeyPath<A, Prop>>.self)

  var b = \A.[sub]
  expect(&b, toHaveType: Exactly<WritableKeyPath<A, A>>.self)

  var c = \A.[sub].property
  expect(&c, toHaveType: Exactly<WritableKeyPath<A, Prop>>.self)

  var d = \A.optProperty?
  expect(&d, toHaveType: Exactly<KeyPath<A, Prop?>>.self)

  var e = \A.optProperty?[sub]
  expect(&e, toHaveType: Exactly<KeyPath<A, A?>>.self)

  var f = \A.optProperty!
  expect(&f, toHaveType: Exactly<WritableKeyPath<A, Prop>>.self)

  var g = \A.property[optSub]?.optProperty![sub]
  expect(&g, toHaveType: Exactly<KeyPath<A, A?>>.self)

  var h = \[A].property
  expect(&h, toHaveType: Exactly<KeyPath<[A], Prop>>.self)

  var i = \[A].property.nonMutatingProperty
  expect(&i, toHaveType: Exactly<ReferenceWritableKeyPath<[A], B>>.self)

  var j = \[A].[x]
  expect(&j, toHaveType: Exactly<WritableKeyPath<[A], A>>.self)

  var k = \[A: B].[A()]
  expect(&k, toHaveType: Exactly<WritableKeyPath<[A: B], B?>>.self)

  var l = \C<A>.value
  expect(&l, toHaveType: Exactly<WritableKeyPath<C<A>, A>>.self)

  // expected-error@+1{{generic parameter 'T' could not be inferred}}
  _ = \C.value

  // expected-error@+1{{}}
  _ = \(() -> ()).noMember

  let _: PartialKeyPath<A> = \.property
  let _: KeyPath<A, Prop> = \.property
  let _: WritableKeyPath<A, Prop> = \.property
  // expected-error@+1{{ambiguous}} (need to improve diagnostic)
  let _: ReferenceWritableKeyPath<A, Prop> = \.property

  // FIXME: shouldn't be ambiguous
  // expected-error@+1{{ambiguous}}
  let _: PartialKeyPath<A> = \.[sub]
  let _: KeyPath<A, A> = \.[sub]
  let _: WritableKeyPath<A, A> = \.[sub]
  // expected-error@+1{{ambiguous}} (need to improve diagnostic)
  let _: ReferenceWritableKeyPath<A, A> = \.[sub]

  let _: PartialKeyPath<A> = \.optProperty?
  let _: KeyPath<A, Prop?> = \.optProperty?
  // expected-error@+1{{cannot convert}}
  let _: WritableKeyPath<A, Prop?> = \.optProperty?
  // expected-error@+1{{cannot convert}}
  let _: ReferenceWritableKeyPath<A, Prop?> = \.optProperty?

  let _: PartialKeyPath<A> = \.optProperty?[sub]
  let _: KeyPath<A, A?> = \.optProperty?[sub]
  // expected-error@+1{{cannot convert}}
  let _: WritableKeyPath<A, A?> = \.optProperty?[sub]
  // expected-error@+1{{cannot convert}}
  let _: ReferenceWritableKeyPath<A, A?> = \.optProperty?[sub]

  let _: KeyPath<A, Prop> = \.optProperty!
  let _: KeyPath<A, Prop> = \.optLetProperty!
  let _: KeyPath<A, Prop?> = \.property[optSub]?.optProperty!
  let _: KeyPath<A, A?> = \.property[optSub]?.optProperty![sub]

  let _: PartialKeyPath<C<A>> = \.value
  let _: KeyPath<C<A>, A> = \.value
  let _: WritableKeyPath<C<A>, A> = \.value
  // expected-error@+1{{ambiguous}} (need to improve diagnostic)
  let _: ReferenceWritableKeyPath<C<A>, A> = \.value

  let _: PartialKeyPath<C<A>> = \C.value
  let _: KeyPath<C<A>, A> = \C.value
  let _: WritableKeyPath<C<A>, A> = \C.value
  // expected-error@+1{{cannot convert}}
  let _: ReferenceWritableKeyPath<C<A>, A> = \C.value

  let _: PartialKeyPath<Prop> = \.nonMutatingProperty
  let _: KeyPath<Prop, B> = \.nonMutatingProperty
  let _: WritableKeyPath<Prop, B> = \.nonMutatingProperty
  let _: ReferenceWritableKeyPath<Prop, B> = \.nonMutatingProperty

  var m = [\A.property, \A.[sub], \A.optProperty!]
  expect(&m, toHaveType: Exactly<[PartialKeyPath<A>]>.self)

  // FIXME: shouldn't be ambiguous
  // expected-error@+1{{ambiguous}}
  var n = [\A.property, \.optProperty, \.[sub], \.optProperty!]
  expect(&n, toHaveType: Exactly<[PartialKeyPath<A>]>.self)

  // FIXME: shouldn't be ambiguous
  // expected-error@+1{{ambiguous}}
  let _: [PartialKeyPath<A>] = [\.property, \.optProperty, \.[sub], \.optProperty!]

  var o = [\A.property, \C<A>.value]
  expect(&o, toHaveType: Exactly<[AnyKeyPath]>.self)

  let _: AnyKeyPath = \A.property
  let _: AnyKeyPath = \C<A>.value
  let _: AnyKeyPath = \.property // expected-error{{ambiguous}}
  let _: AnyKeyPath = \C.value // expected-error{{cannot convert}} (need to improve diagnostic)
  let _: AnyKeyPath = \.value // expected-error{{ambiguous}}

  let _ = \Prop.[nonHashableSub] // expected-error{{subscript index of type 'NonHashableSub' in a key path must be Hashable}}
  let _ = \Prop.[sub, sub]
  let _ = \Prop.[sub, nonHashableSub] // expected-error{{subscript index of type 'NonHashableSub' in a key path must be Hashable}}

  let _ = \C<Int>.[]
  let _ = \C<Int>.[sub]
  let _ = \C<Int>.[noHashableConstraint: sub]
  let _ = \C<Int>.[noHashableConstraint: nonHashableSub] // expected-error{{subscript index of type 'NonHashableSub' in a key path must be Hashable}}
}

func testKeyPathInGenericContext<H: Hashable, X>(hashable: H, anything: X) {
  let _ = \C<Int>.[hashable]
  let _ = \C<Int>.[noHashableConstraint: hashable]
  let _ = \C<Int>.[noHashableConstraint: anything] // expected-error{{subscript index of type 'X' in a key path must be Hashable}}
}

func testDisembodiedStringInterpolation(x: Int) {
  \(x) // expected-error{{string interpolation}} expected-error{{}}
  \(x, radix: 16) // expected-error{{string interpolation}} expected-error{{}}

  _ = \(Int, Int).0 // expected-error{{cannot reference tuple elements}}
}

func testNoComponents() {
  let _: KeyPath<A, A> = \A // expected-error{{must have at least one component}}
  let _: KeyPath<C, A> = \C // expected-error{{must have at least one component}} expected-error{{}}
}

struct TupleStruct {
  var unlabeled: (Int, String)
  var labeled: (foo: Int, bar: String)
}

func tupleComponent() {
  // TODO: Customized diagnostic
  let _ = \(Int, String).0 // expected-error{{}}
  let _ = \(Int, String).1 // expected-error{{}}
  let _ = \TupleStruct.unlabeled.0 // expected-error{{}}
  let _ = \TupleStruct.unlabeled.1 // expected-error{{}}

  let _ = \(foo: Int, bar: String).0 // expected-error{{}}
  let _ = \(foo: Int, bar: String).1 // expected-error{{}}
  let _ = \(foo: Int, bar: String).foo // expected-error{{}}
  let _ = \(foo: Int, bar: String).bar // expected-error{{}}
  let _ = \TupleStruct.labeled.0 // expected-error{{}}
  let _ = \TupleStruct.labeled.1 // expected-error{{}}
  let _ = \TupleStruct.labeled.foo // expected-error{{}}
  let _ = \TupleStruct.labeled.bar // expected-error{{}}
}

struct Z { }

func testKeyPathSubscript(readonly: Z, writable: inout Z,
                          kp: KeyPath<Z, Int>,
                          wkp: WritableKeyPath<Z, Int>,
                          rkp: ReferenceWritableKeyPath<Z, Int>) {
  var sink: Int
  sink = readonly[keyPath: kp]
  sink = writable[keyPath: kp]
  sink = readonly[keyPath: wkp]
  sink = writable[keyPath: wkp]
  sink = readonly[keyPath: rkp]
  sink = writable[keyPath: rkp]

  readonly[keyPath: kp] = sink // expected-error{{cannot assign to immutable}}
  writable[keyPath: kp] = sink // expected-error{{cannot assign to immutable}}
  readonly[keyPath: wkp] = sink // expected-error{{cannot assign to immutable}}
  writable[keyPath: wkp] = sink
  readonly[keyPath: rkp] = sink
  writable[keyPath: rkp] = sink

  let pkp: PartialKeyPath = rkp

  var anySink1 = readonly[keyPath: pkp]
  expect(&anySink1, toHaveType: Exactly<Any>.self)
  var anySink2 = writable[keyPath: pkp]
  expect(&anySink2, toHaveType: Exactly<Any>.self)

  readonly[keyPath: pkp] = anySink1 // expected-error{{cannot assign to immutable}}
  writable[keyPath: pkp] = anySink2 // expected-error{{cannot assign to immutable}}

  let akp: AnyKeyPath = pkp

  var anyqSink1 = readonly[keyPath: akp]
  expect(&anyqSink1, toHaveType: Exactly<Any?>.self)
  var anyqSink2 = writable[keyPath: akp]
  expect(&anyqSink2, toHaveType: Exactly<Any?>.self)

  readonly[keyPath: akp] = anyqSink1 // expected-error{{cannot assign to immutable}}
  writable[keyPath: akp] = anyqSink2 // expected-error{{cannot assign to immutable}}
}

struct ZwithSubscript {
  subscript(keyPath kp: KeyPath<ZwithSubscript, Int>) -> Int { return 0 }
  subscript(keyPath kp: WritableKeyPath<ZwithSubscript, Int>) -> Int { return 0 }
  subscript(keyPath kp: ReferenceWritableKeyPath<ZwithSubscript, Int>) -> Int { return 0 }
  subscript(keyPath kp: PartialKeyPath<ZwithSubscript>) -> Any { return 0 }
}

struct NotZ {}

func testKeyPathSubscript(readonly: ZwithSubscript, writable: inout ZwithSubscript,
                          wrongType: inout NotZ,
                          kp: KeyPath<ZwithSubscript, Int>,
                          wkp: WritableKeyPath<ZwithSubscript, Int>,
                          rkp: ReferenceWritableKeyPath<ZwithSubscript, Int>) {
  var sink: Int
  sink = readonly[keyPath: kp]
  sink = writable[keyPath: kp]
  sink = readonly[keyPath: wkp]
  sink = writable[keyPath: wkp]
  sink = readonly[keyPath: rkp]
  sink = writable[keyPath: rkp]

  readonly[keyPath: kp] = sink // expected-error{{cannot assign through subscript: subscript is get-only}}
  writable[keyPath: kp] = sink // expected-error{{cannot assign through subscript: subscript is get-only}}
  readonly[keyPath: wkp] = sink // expected-error{{cannot assign through subscript: subscript is get-only}}
  // FIXME: silently falls back to keypath application, which seems inconsistent
  writable[keyPath: wkp] = sink
  // FIXME: silently falls back to keypath application, which seems inconsistent
  readonly[keyPath: rkp] = sink
  // FIXME: silently falls back to keypath application, which seems inconsistent
  writable[keyPath: rkp] = sink

  let pkp: PartialKeyPath = rkp

  var anySink1 = readonly[keyPath: pkp]
  expect(&anySink1, toHaveType: Exactly<Any>.self)
  var anySink2 = writable[keyPath: pkp]
  expect(&anySink2, toHaveType: Exactly<Any>.self)

  readonly[keyPath: pkp] = anySink1 // expected-error{{cannot assign through subscript: subscript is get-only}}
  writable[keyPath: pkp] = anySink2 // expected-error{{cannot assign through subscript: subscript is get-only}}

  let akp: AnyKeyPath = pkp

  var anyqSink1 = readonly[keyPath: akp]
  expect(&anyqSink1, toHaveType: Exactly<Any?>.self)
  var anyqSink2 = writable[keyPath: akp]
  expect(&anyqSink2, toHaveType: Exactly<Any?>.self)

  // FIXME: silently falls back to keypath application, which seems inconsistent
  readonly[keyPath: akp] = anyqSink1 // expected-error{{cannot assign to immutable}}
  // FIXME: silently falls back to keypath application, which seems inconsistent
  writable[keyPath: akp] = anyqSink2 // expected-error{{cannot assign to immutable}}

  _ = wrongType[keyPath: kp] // expected-error{{cannot be applied}}
  _ = wrongType[keyPath: wkp] // expected-error{{cannot be applied}}
  _ = wrongType[keyPath: rkp] // expected-error{{cannot be applied}}
  _ = wrongType[keyPath: pkp] // expected-error{{cannot be applied}}
  _ = wrongType[keyPath: akp]
}

func testKeyPathSubscriptMetatype(readonly: Z.Type, writable: inout Z.Type,
                                  kp: KeyPath<Z.Type, Int>,
                                  wkp: WritableKeyPath<Z.Type, Int>,
                                  rkp: ReferenceWritableKeyPath<Z.Type, Int>) {
  var sink: Int
  sink = readonly[keyPath: kp]
  sink = writable[keyPath: kp]
  sink = readonly[keyPath: wkp]
  sink = writable[keyPath: wkp]
  sink = readonly[keyPath: rkp]
  sink = writable[keyPath: rkp]

  readonly[keyPath: kp] = sink // expected-error{{cannot assign to immutable}}
  writable[keyPath: kp] = sink // expected-error{{cannot assign to immutable}}
  readonly[keyPath: wkp] = sink // expected-error{{cannot assign to immutable}}
  writable[keyPath: wkp] = sink
  readonly[keyPath: rkp] = sink
  writable[keyPath: rkp] = sink
}

func testKeyPathSubscriptTuple(readonly: (Z,Z), writable: inout (Z,Z),
                               kp: KeyPath<(Z,Z), Int>,
                               wkp: WritableKeyPath<(Z,Z), Int>,
                               rkp: ReferenceWritableKeyPath<(Z,Z), Int>) {
  var sink: Int
  sink = readonly[keyPath: kp]
  sink = writable[keyPath: kp]
  sink = readonly[keyPath: wkp]
  sink = writable[keyPath: wkp]
  sink = readonly[keyPath: rkp]
  sink = writable[keyPath: rkp]

  readonly[keyPath: kp] = sink // expected-error{{cannot assign to immutable}}
  writable[keyPath: kp] = sink // expected-error{{cannot assign to immutable}}
  readonly[keyPath: wkp] = sink // expected-error{{cannot assign to immutable}}
  writable[keyPath: wkp] = sink
  readonly[keyPath: rkp] = sink
  writable[keyPath: rkp] = sink
}

func testKeyPathSubscriptLValue(base: Z, kp: inout KeyPath<Z, Z>) {
  _ = base[keyPath: kp]
}

func testKeyPathSubscriptExistentialBase(concreteBase: inout B,
                                     existentialBase: inout P,
                                     kp: KeyPath<P, String>,
                                     wkp: WritableKeyPath<P, String>,
                                     rkp: ReferenceWritableKeyPath<P, String>,
                                     pkp: PartialKeyPath<P>,
                                     s: String) {
  _ = concreteBase[keyPath: kp]
  _ = concreteBase[keyPath: wkp]
  _ = concreteBase[keyPath: rkp]
  _ = concreteBase[keyPath: pkp]

  concreteBase[keyPath: kp] = s // expected-error{{}}
  concreteBase[keyPath: wkp] = s // expected-error{{}}
  concreteBase[keyPath: rkp] = s
  concreteBase[keyPath: pkp] = s // expected-error{{}}

  _ = existentialBase[keyPath: kp]
  _ = existentialBase[keyPath: wkp]
  _ = existentialBase[keyPath: rkp]
  _ = existentialBase[keyPath: pkp]

  existentialBase[keyPath: kp] = s // expected-error{{}}
  existentialBase[keyPath: wkp] = s
  existentialBase[keyPath: rkp] = s
  existentialBase[keyPath: pkp] = s // expected-error{{}}
}

struct AA {
  subscript(x: Int) -> Int { return x }
  subscript(labeled x: Int) -> Int { return x }
  var c: CC? = CC()
}

class CC {
  var i = 0
}

func testKeyPathOptional() {
  _ = \AA.c?.i
  _ = \AA.c!.i

  // SR-6198
  let path: KeyPath<CC,Int>! = \CC.i
  let cc = CC()
  _ = cc[keyPath: path]
}

func testLiteralInAnyContext() {
  let _: AnyKeyPath = \A.property
  let _: AnyObject = \A.property
  let _: Any = \A.property
  let _: Any? = \A.property
}

func testMoreGeneralContext<T, U>(_: KeyPath<T, U>, with: T.Type) {}

func testLiteralInMoreGeneralContext() {
  testMoreGeneralContext(\.property, with: A.self)
}

func testLabeledSubscript() {
  let _: KeyPath<AA, Int> = \AA.[labeled: 0]
  let _: KeyPath<AA, Int> = \.[labeled: 0]
  let k = \AA.[labeled: 0]

  // TODO: These ought to work without errors.
  let _ = \AA.[keyPath: k] // expected-error{{}}
  let _ = \AA.[keyPath: \AA.[labeled: 0]] // expected-error{{}}
}

func testInvalidKeyPathComponents() {
  let _ = \.{return 0} // expected-error* {{}}
}

class X {
  class var a: Int { return 1 }
  static var b = 2
}

func testStaticKeyPathComponent() {
  _ = \X.a // expected-error{{}}
  _ = \X.Type.a // expected-error{{cannot refer to static member}}
  _ = \X.b // expected-error{{}}
  _ = \X.Type.b // expected-error{{cannot refer to static member}}
}

class Bass: Hashable {
  static func ==(_: Bass, _: Bass) -> Bool { return false }
  var hashValue: Int { return 0 }
}

class Treble: Bass { }

struct BassSubscript {
  subscript(_: Bass) -> Int { fatalError() }
  subscript(_: @autoclosure () -> String) -> Int { fatalError() }
}

func testImplicitConversionInSubscriptIndex() {
  _ = \BassSubscript.[Treble()]
  _ = \BassSubscript.["hello"] // expected-error{{must be Hashable}}
}

// SR-6106
func sr6106() {
  class B {}
  class A {
    var b: B? = nil
  }
  class C {
    var a: A?
    func myFunc() {
      let _ = \C.a?.b
    }
  }
}

// SR-6744
func sr6744() {
    struct ABC {
        let value: Int
        func value(adding i: Int) -> Int { return value + i }
    }

    let abc = ABC(value: 0)
    func get<T>(for kp: KeyPath<ABC, T>) -> T {
        return abc[keyPath: kp]
    }
    _ = get(for: \.value)
}

struct VisibilityTesting {
  private(set) var x: Int
  fileprivate(set) var y: Int
  let z: Int

  // Key path exprs should not get special dispensation to write to lets
  // in init contexts
  init() {
    var xRef = \VisibilityTesting.x
    var yRef = \VisibilityTesting.y
    var zRef = \VisibilityTesting.z
    expect(&xRef,
      toHaveType: Exactly<WritableKeyPath<VisibilityTesting, Int>>.self)
    expect(&yRef,
      toHaveType: Exactly<WritableKeyPath<VisibilityTesting, Int>>.self)
    expect(&zRef,
      toHaveType: Exactly<KeyPath<VisibilityTesting, Int>>.self)
  }

  func inPrivateContext() {
    var xRef = \VisibilityTesting.x
    var yRef = \VisibilityTesting.y
    var zRef = \VisibilityTesting.z
    expect(&xRef,
      toHaveType: Exactly<WritableKeyPath<VisibilityTesting, Int>>.self)
    expect(&yRef,
      toHaveType: Exactly<WritableKeyPath<VisibilityTesting, Int>>.self)
    expect(&zRef,
      toHaveType: Exactly<KeyPath<VisibilityTesting, Int>>.self)
  }
}

struct VisibilityTesting2 {
  func inFilePrivateContext() {
    var xRef = \VisibilityTesting.x
    var yRef = \VisibilityTesting.y
    var zRef = \VisibilityTesting.z
    expect(&xRef,
      toHaveType: Exactly<KeyPath<VisibilityTesting, Int>>.self)
    expect(&yRef,
      toHaveType: Exactly<WritableKeyPath<VisibilityTesting, Int>>.self)
    expect(&zRef,
      toHaveType: Exactly<KeyPath<VisibilityTesting, Int>>.self)
  }
}

protocol PP {}
class Base : PP { var i: Int = 0 }
class Derived : Base {}

func testSubtypeKeypathClass(_ keyPath: ReferenceWritableKeyPath<Base, Int>) {
  testSubtypeKeypathClass(\Derived.i)
}

func testSubtypeKeypathProtocol(_ keyPath: ReferenceWritableKeyPath<PP, Int>) {
  testSubtypeKeypathProtocol(\Base.i) // expected-error {{type 'PP' has no member 'i'}}
}

func testSyntaxErrors() { // expected-note{{}}
  _ = \.  ; // expected-error{{expected member name following '.'}}
  _ = \.a ;
  _ = \[a ;
  _ = \[a];
  _ = \?  ;
  _ = \!  ;
  _ = \.  ; // expected-error{{expected member name following '.'}}
  _ = \.a ;
  _ = \[a ;
  _ = \[a,;
  _ = \[a:;
  _ = \[a];
  _ = \.a?;
  _ = \.a!;
  _ = \A     ;
  _ = \A,    ;
  _ = \A<    ;
  _ = \A.  ; // expected-error{{expected member name following '.'}}
  _ = \A.a ;
  _ = \A[a ;
  _ = \A[a];
  _ = \A?  ;
  _ = \A!  ;
  _ = \A.  ; // expected-error{{expected member name following '.'}}
  _ = \A.a ;
  _ = \A[a ;
  _ = \A[a,;
  _ = \A[a:;
  _ = \A[a];
  _ = \A.a?;
  _ = \A.a!;
} // expected-error@+1{{}}
