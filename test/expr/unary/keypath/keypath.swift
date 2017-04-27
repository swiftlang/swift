// RUN: %target-swift-frontend -typecheck -parse-as-library -enable-experimental-keypaths  %s -verify

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
  var a = #keyPath2(A, .property)
  expect(&a, toHaveType: Exactly<WritableKeyPath<A, Prop>>.self)

  var b = #keyPath2(A, [sub])
  expect(&b, toHaveType: Exactly<WritableKeyPath<A, A>>.self)

  var c = #keyPath2(A, [sub].property)
  expect(&c, toHaveType: Exactly<WritableKeyPath<A, Prop>>.self)

  var d = #keyPath2(A, .optProperty?)
  expect(&d, toHaveType: Exactly<KeyPath<A, Prop?>>.self)

  var e = #keyPath2(A, .optProperty?[sub])
  expect(&e, toHaveType: Exactly<KeyPath<A, A?>>.self)

  var f = #keyPath2(A, .optProperty!)
  expect(&f, toHaveType: Exactly<WritableKeyPath<A, Prop>>.self)

  var g = #keyPath2(A, .property[optSub]?.optProperty![sub])
  expect(&g, toHaveType: Exactly<KeyPath<A, A?>>.self)

  var h = #keyPath2([A], .property)
  expect(&h, toHaveType: Exactly<KeyPath<[A], Prop>>.self)

  var i = #keyPath2([A], .property.nonMutatingProperty)
  expect(&i, toHaveType: Exactly<ReferenceWritableKeyPath<[A], B>>.self)

  var j = #keyPath2([A], [x])
  expect(&j, toHaveType: Exactly<WritableKeyPath<[A], A>>.self)

  var k = #keyPath2([A: B], [A()])
  expect(&k, toHaveType: Exactly<WritableKeyPath<[A: B], B?>>.self)

  var l = #keyPath2(C<A>, .value)
  expect(&l, toHaveType: Exactly<WritableKeyPath<C<A>, A>>.self)

  // expected-error@+1{{generic parameter 'T' could not be inferred}}
  _ = #keyPath2(C, .value)

  // expected-error@+1{{}}
  _ = #keyPath2(() -> (), .noMember)

  // FIXME crash let _: PartialKeyPath<A> = #keyPath2(.property)
  let _: KeyPath<A, Prop> = #keyPath2(.property)
  let _: WritableKeyPath<A, Prop> = #keyPath2(.property)
  // expected-error@+1{{ambiguous}} (need to improve diagnostic)
  let _: ReferenceWritableKeyPath<A, Prop> = #keyPath2(.property)

  // FIXME crash let _: PartialKeyPath<A> = #keyPath2([sub])
  // FIXME should resolve: expected-error@+1{{}}
  let _: KeyPath<A, A> = #keyPath2([sub])
  // FIXME should resolve: expected-error@+1{{}}
  let _: WritableKeyPath<A, A> = #keyPath2([sub])
  // expected-error@+1{{ambiguous}} (need to improve diagnostic)
  let _: ReferenceWritableKeyPath<A, A> = #keyPath2([sub])

  // FIXME crash let _: PartialKeyPath<A> = #keyPath2(.optProperty?)
  let _: KeyPath<A, Prop?> = #keyPath2(.optProperty?)
  // expected-error@+1{{cannot convert}}
  let _: WritableKeyPath<A, Prop?> = #keyPath2(.optProperty?)
  // expected-error@+1{{cannot convert}}
  let _: ReferenceWritableKeyPath<A, Prop?> = #keyPath2(.optProperty?)

  // FIXME crash let _: PartialKeyPath<A> = #keyPath2(.optProperty?[sub])
  let _: KeyPath<A, A?> = #keyPath2(.optProperty?[sub])
  // expected-error@+1{{cannot convert}}
  let _: WritableKeyPath<A, A?> = #keyPath2(.optProperty?[sub])
  // expected-error@+1{{cannot convert}}
  let _: ReferenceWritableKeyPath<A, A?> = #keyPath2(.optProperty?[sub])

  // FIXME should resolve: expected-error@+1{{}}
  let _: KeyPath<A, Prop> = #keyPath2(.optProperty!)
  // FIXME should resolve: expected-error@+1{{}}
  let _: KeyPath<A, Prop?> = #keyPath2(.property[optSub]?.optProperty![sub])

  // FIXME crash let _: PartialKeyPath<C<A>> = #keyPath2(.value)
  let _: KeyPath<C<A>, A> = #keyPath2(.value)
  let _: WritableKeyPath<C<A>, A> = #keyPath2(.value)
  // expected-error@+1{{ambiguous}} (need to improve diagnostic)
  let _: ReferenceWritableKeyPath<C<A>, A> = #keyPath2(.value)

  // FIXME crash let _: PartialKeyPath<C<A>> = #keyPath2(C, .value)
  let _: KeyPath<C<A>, A> = #keyPath2(C, .value)
  let _: WritableKeyPath<C<A>, A> = #keyPath2(C, .value)
  // expected-error@+1{{cannot convert}}
  let _: ReferenceWritableKeyPath<C<A>, A> = #keyPath2(C, .value)

  // FIXME crash let _: PartialKeyPath<Prop> = #keyPath2(.nonMutatingProperty)
  let _: KeyPath<Prop, B> = #keyPath2(.nonMutatingProperty)
  let _: WritableKeyPath<Prop, B> = #keyPath2(.nonMutatingProperty)
  let _: ReferenceWritableKeyPath<Prop, B> = #keyPath2(.nonMutatingProperty)
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
  _ = #keyPath2(.  ; // expected-error{{expected property or type name}}
  _ = #keyPath2(.a ;
  _ = #keyPath2([a ;
  _ = #keyPath2([a];
  _ = #keyPath2(?  ;
  _ = #keyPath2(!  ;
  _ = #keyPath2(.  ;
  _ = #keyPath2(.a ;
  _ = #keyPath2([a ;
  _ = #keyPath2([a,;
  _ = #keyPath2([a:;
  _ = #keyPath2([a];
  _ = #keyPath2(.a?;
  _ = #keyPath2(.a!;
  _ = #keyPath2(A     ;
  _ = #keyPath2(A,    ;
  _ = #keyPath2(A<    ;
  _ = #keyPath2(A, .  ;
  _ = #keyPath2(A, .a ;
  _ = #keyPath2(A, [a ;
  _ = #keyPath2(A, [a];
  _ = #keyPath2(A, ?  ;
  _ = #keyPath2(A, !  ;
  _ = #keyPath2(A, .  ;
  _ = #keyPath2(A, .a ;
  _ = #keyPath2(A, [a ;
  _ = #keyPath2(A, [a,;
  _ = #keyPath2(A, [a:;
  _ = #keyPath2(A, [a];
  _ = #keyPath2(A, .a?;
  _ = #keyPath2(A, .a!;
} // expected-error@+1{{}}
