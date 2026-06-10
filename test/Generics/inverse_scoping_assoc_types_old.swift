// RUN: %target-typecheck-verify-swift  -enable-experimental-feature SuppressedAssociatedTypes

// REQUIRES: swift_feature_SuppressedAssociatedTypes

protocol NoCopyReq: ~Copyable {}

protocol P {
  associatedtype AT where Self: ~Copyable // expected-error {{constraint with subject type of 'Self' is not supported; consider adding requirement to protocol inheritance clause instead}}

  associatedtype Bob where Alice: NoCopyReq & ~Copyable // expected-warning {{experimental feature 'SuppressedAssociatedTypes' is deprecated}}
  associatedtype Alice where Bob: ~Copyable // expected-warning {{experimental feature 'SuppressedAssociatedTypes' is deprecated}}
}

protocol Q<Primary>: ~Copyable {
  associatedtype Primary: ~Copyable // expected-warning {{experimental feature 'SuppressedAssociatedTypes' is deprecated}}
  associatedtype Secondary: ~Copyable // expected-warning {{experimental feature 'SuppressedAssociatedTypes' is deprecated}}
}

extension Q {
  func testCopyability(_ a: Self.Primary, // expected-error {{parameter of noncopyable type 'Self.Primary}} // expected-note 3{{}}
                       _ b: Self.Secondary) {} // expected-error {{parameter of noncopyable type 'Self.Secondary}} // expected-note 3{{}}

  func testCopyability2(_ s: Self) {}
}

func genericFunc<T: Q>(_ t: T,
                       _ a: T.Primary, // expected-error {{parameter of noncopyable type 'T.Primary}} // expected-note 3{{}}
                       _ b: T.Secondary) {} // expected-error {{parameter of noncopyable type 'T.Secondary}} // expected-note 3{{}}


protocol Foo<Inner>: ~Copyable {
  associatedtype Inner: ~Copyable  // expected-warning {{experimental feature 'SuppressedAssociatedTypes' is deprecated}}
  func getInner() -> Inner
}

struct NC: ~Copyable {}

struct WitnessFoo<Inner: ~Copyable>: Foo {
  func getInner() -> Inner { fatalError("todo") }
}

extension WitnessFoo where Inner: ~Copyable {
  func asFoo() -> some Foo<Inner> {
    return self
  }
}

func createNCInner<Inner: ~Copyable>(_ t: Inner.Type) -> some Foo<Inner> {
    return WitnessFoo<Inner>()
}

func createCopyableInner<Inner>(_ t: Inner.Type) -> some Foo<Inner> { // expected-note {{'where Inner: Copyable' is implicit here}}
  return WitnessFoo<Inner>()
}

func expectC<T>(_ t: T) {} // expected-note {{'where T: Copyable' is implicit here}}
func permitNC<T: ~Copyable>(_ t: borrowing T) {}

func test() {
  _ = createCopyableInner(Int.self)
  _ = createCopyableInner(NC.self) // expected-error {{global function 'createCopyableInner' requires that 'NC' conform to 'Copyable'}}

  expectC(createNCInner(Int.self).getInner())
  expectC(createNCInner(NC.self).getInner()) // expected-error {{global function 'expectC' requires that 'NC' conform to 'Copyable'}}

  permitNC(createNCInner(Int.self).getInner())
  permitNC(createNCInner(NC.self).getInner())
}
