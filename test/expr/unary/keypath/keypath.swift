// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -parse-as-library  %s -verify

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
  let _: KeyPath<A, A> = #keyPath2([sub])
  let _: KeyPath<A, Prop?> = #keyPath2(.optProperty?)
  let _: KeyPath<A, A?> = #keyPath2(.optProperty?[sub])
  let _: KeyPath<A, Prop> = #keyPath2(.optProperty!)
  // TODO: why is this unresolved?
  // expected-error@+1{{}}
  let _: KeyPath<A, Prop?> = #keyPath2(.property[optSub]?.optProperty![sub])
  let _: KeyPath<C<A>, A> = #keyPath2(.value)
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
