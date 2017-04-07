// RUN: %target-swift-frontend -typecheck -parse-as-library  %s -verify

struct Sub {}
struct OptSub {}
struct Prop {
  subscript(sub: Sub) -> A { get { return A() } set { } }
  subscript(optSub: OptSub) -> A? { get { return A() } set { } }
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

func testKeyPath(sub: Sub, optSub: OptSub, x: Int) {
  _ = #keyPath2(A, .property)
  _ = #keyPath2(A, [sub])
  _ = #keyPath2(A, [sub].property)
  _ = #keyPath2(A, .optProperty?)
  _ = #keyPath2(A, .optProperty?[sub])
  _ = #keyPath2(A, .optProperty!)
  _ = #keyPath2(A, .property[optSub]?.optProperty![sub])
  _ = #keyPath2([A], .property)
  _ = #keyPath2([A], [x])
  _ = #keyPath2([A: B], [A()])
  _ = #keyPath2(C<A>, .value)

  // expected-error@+1{{}}
  _ = #keyPath2(() -> (), .noMember)

  let _: KeyPath<A, Prop> = #keyPath2(.property)
  // TODO: why is this unresolved?
  // expected-error@+1{{}}
  let _: KeyPath<A, A> = #keyPath2([sub])
  let _: KeyPath<A, Prop?> = #keyPath2(.optProperty?)
  let _: KeyPath<A, A?> = #keyPath2(.optProperty?[sub])
  let _: KeyPath<A, Prop> = #keyPath2(.optProperty!)
  // TODO: why is this unresolved?
  // expected-error@+1{{}}
  let _: KeyPath<A, Prop?> = #keyPath2(.property[optSub]?.optProperty![sub])
  let _: KeyPath<C<A>, A> = #keyPath2(.value)
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
