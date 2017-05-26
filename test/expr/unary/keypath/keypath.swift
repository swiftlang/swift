// RUN: %target-swift-frontend -typecheck -parse-as-library -enable-experimental-keypath-components  %s -verify

struct Sub {}
struct OptSub {}
struct Prop {
  subscript(sub: Sub) -> A { get { return A() } set { } }
  subscript(optSub: OptSub) -> A? { get { return A() } set { } }

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
  subscript(sub: Sub) -> T { get { return value } set { } }
  subscript<U>(sub: U) -> U { get { return sub } set { } }
}

extension Array where Element == A {
  var property: Prop { fatalError() }
}

struct Exactly<T> {}

func expect<T>(_ x: inout T, toHaveType _: Exactly<T>.Type) {}

func testKeyPath(sub: Sub, optSub: OptSub, x: Int) {
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

func testSyntaxErrors() { // expected-note{{}}
  // TODO: recovery
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
