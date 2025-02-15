// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen

// NOTE: Comparing -dump-ast instead of -dump-parse because ASTGen  generates
// KeyPathExpr differently. In C++ parser the root expression is parsed as a
// normal expression, but in SwiftParser it's parsed as a TypeSyntax, so ASTGen
// generates it as TypeExpr. But they are normalized in PreCheckTarget and
// should end up with the same type-checked AST.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-dump-ast -enable-experimental-feature ParserASTGen -verify \
// RUN:   | %sanitize-address > %t/astgen.ast
// RUN: not %target-swift-frontend-dump-ast \
// RUN:   | %sanitize-address > %t/cpp-parser.ast

// RUN: %diff -u %t/astgen.ast %t/cpp-parser.ast

struct Sub: Hashable {
  static func ==(_: Sub, _: Sub) -> Bool { return true }
  func hash(into hasher: inout Hasher) {}
}
struct OptSub: Hashable {
  static func ==(_: OptSub, _: OptSub) -> Bool { return true }
  func hash(into hasher: inout Hasher) {}
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
  func hash(into hasher: inout Hasher) { fatalError() }
}
struct B {}
struct C<T> { // expected-note 2 {{'T' declared as parameter to type 'C'}}
  var value: T
  subscript() -> T { get { return value } }
  subscript(sub: Sub) -> T { get { return value } set { } }
  subscript<U: Hashable>(sub: U) -> U { get { return sub } set { } }
  subscript<X>(noHashableConstraint sub: X) -> X { get { return sub } set { } }
}

/* FIXME: @available is not implmented in ASTGen.
struct Unavailable {
  @available(*, unavailable)
  var unavailableProperty: Int { 0 }
  // xpected-note@-1 {{'unavailableProperty' has been explicitly marked unavailable here}}

  @available(*, unavailable)
  subscript(x: Sub) -> Int { get { } set { } }
  // xpected-note@-1 {{'subscript(_:)' has been explicitly marked unavailable here}}
}

struct Deprecated {
  @available(*, deprecated)
  var deprecatedProperty: Int

  @available(*, deprecated)
  subscript(x: Sub) -> Int { get { } set { } }
}

@available(*, deprecated)
func getDeprecatedSub() -> Sub {
  return Sub()
}
*/

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

  let _: (A) -> Prop = \.property
  let _: (A) -> Prop? = \.property
  let _: PartialKeyPath<A> = \.property
  let _: KeyPath<A, Prop> = \.property
  let _: WritableKeyPath<A, Prop> = \.property
  let _: ReferenceWritableKeyPath<A, Prop> = \.property
  //expected-error@-1 {{cannot convert key path type 'WritableKeyPath<A, Prop>' to contextual type 'ReferenceWritableKeyPath<A, Prop>'}}

  let _: (A) -> A? = \.[sub]
  let _: (A) -> A = \.[sub]
  let _: PartialKeyPath<A> = \.[sub]
  let _: KeyPath<A, A> = \.[sub]
  let _: WritableKeyPath<A, A> = \.[sub]
  let _: ReferenceWritableKeyPath<A, A> = \.[sub]
  //expected-error@-1 {{cannot convert key path type 'WritableKeyPath<A, A>' to contextual type 'ReferenceWritableKeyPath<A, A>'}}

  let _: (A) -> Prop?? = \.optProperty?
  let _: (A) -> Prop? = \.optProperty?
  let _: PartialKeyPath<A> = \.optProperty?
  let _: KeyPath<A, Prop?> = \.optProperty?
  // expected-error@+1{{cannot convert}}
  let _: WritableKeyPath<A, Prop?> = \.optProperty?
  // expected-error@+1{{cannot convert}}
  let _: ReferenceWritableKeyPath<A, Prop?> = \.optProperty?

  let _: (A) -> A?? = \.optProperty?[sub]
  let _: (A) -> A? = \.optProperty?[sub]
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

  let _: (C<A>) -> A? = \.value
  let _: (C<A>) -> A = \.value
  let _: PartialKeyPath<C<A>> = \.value
  let _: KeyPath<C<A>, A> = \.value
  let _: WritableKeyPath<C<A>, A> = \.value
  let _: ReferenceWritableKeyPath<C<A>, A> = \.value
  // expected-error@-1 {{cannot convert key path type 'WritableKeyPath<C<A>, A>' to contextual type 'ReferenceWritableKeyPath<C<A>, A>'}}

  let _: (C<A>) -> A? = \C.value
  let _: (C<A>) -> A = \C.value
  let _: PartialKeyPath<C<A>> = \C.value
  let _: KeyPath<C<A>, A> = \C.value
  let _: WritableKeyPath<C<A>, A> = \C.value
  // expected-error@+1{{cannot convert}}
  let _: ReferenceWritableKeyPath<C<A>, A> = \C.value

  let _: (Prop) -> B? = \.nonMutatingProperty
  let _: (Prop) -> B = \.nonMutatingProperty
  let _: PartialKeyPath<Prop> = \.nonMutatingProperty
  let _: KeyPath<Prop, B> = \.nonMutatingProperty
  let _: WritableKeyPath<Prop, B> = \.nonMutatingProperty
  let _: ReferenceWritableKeyPath<Prop, B> = \.nonMutatingProperty

  var m = [\A.property, \A.[sub], \A.optProperty!]
  expect(&m, toHaveType: Exactly<[PartialKeyPath<A>]>.self)

  // \.optProperty returns an optional of Prop and `\.[sub]` returns `A`, all this unifies into `[PartialKeyPath<A>]`
  var n = [\A.property, \.optProperty, \.[sub], \.optProperty!]
  expect(&n, toHaveType: Exactly<[PartialKeyPath<A>]>.self)

  let _: [PartialKeyPath<A>] = [\.property, \.optProperty, \.[sub], \.optProperty!]

  var o = [\A.property, \C<A>.value]
  expect(&o, toHaveType: Exactly<[AnyKeyPath]>.self)

  let _: AnyKeyPath = \A.property
  let _: AnyKeyPath = \C<A>.value
  let _: AnyKeyPath = \.property // expected-error {{'AnyKeyPath' does not provide enough context for root type to be inferred; consider explicitly specifying a root type}} {{24-24=<#Root#>}}
  let _: AnyKeyPath = \C.value // expected-error{{generic parameter 'T' could not be inferred}}
  let _: AnyKeyPath = \.value // expected-error {{'AnyKeyPath' does not provide enough context for root type to be inferred; consider explicitly specifying a root type}} {{24-24=<#Root#>}}

  let _ = \Prop.[nonHashableSub] // expected-error{{subscript index of type 'NonHashableSub' in a key path must be Hashable}}
  let _ = \Prop.[sub, sub]
  let _ = \Prop.[sub, nonHashableSub] // expected-error{{subscript index of type 'NonHashableSub' in a key path must be Hashable}}

  let _ = \C<Int>.[]
  let _ = \C<Int>.[sub]
  let _ = \C<Int>.[noHashableConstraint: sub]
  let _ = \C<Int>.[noHashableConstraint: nonHashableSub] // expected-error{{subscript index of type 'NonHashableSub' in a key path must be Hashable}}

/* FIXME: @available is not implemented in ASTGen.
  let _ = \Unavailable.unavailableProperty // xpected-error {{'unavailableProperty' is unavailable}}
  let _ = \Unavailable.[sub] // xpected-error {{'subscript(_:)' is unavailable}}

  let _ = \Deprecated.deprecatedProperty // xpected-warning {{'deprecatedProperty' is deprecated}}
  let _ = \Deprecated.[sub] // xpected-warning {{'subscript(_:)' is deprecated}}

  let _ = \A.[getDeprecatedSub()] // xpected-warning {{'getDeprecatedSub()' is deprecated}}
*/
}

func testKeyPathInGenericContext<H: Hashable, X>(hashable: H, anything: X) {
  let _ = \C<Int>.[hashable]
  let _ = \C<Int>.[noHashableConstraint: hashable]
  let _ = \C<Int>.[noHashableConstraint: anything] // expected-error{{subscript index of type 'X' in a key path must be Hashable}}
}

