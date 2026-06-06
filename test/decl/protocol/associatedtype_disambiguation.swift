// RUN: %target-typecheck-verify-swift \
// RUN:   -enable-experimental-feature AssociatedTypeDisambiguation \
// RUN:   -disable-experimental-parser-round-trip

// REQUIRES: swift_feature_AssociatedTypeDisambiguation

// The disambiguation syntax is currently only understood by the C++ parser, so
// the SwiftSyntax round-trip check is disabled above.

protocol Alpha { associatedtype Item }
protocol Beta  { associatedtype Item }

struct Cat {}
struct Dog {}

func wantCat(_: Cat) {}
func wantDog(_: Dog) {}

// MARK: - Distinct per-protocol witnesses on a single type.

struct Bag: Alpha, Beta {
  typealias Alpha.Item = Cat
  typealias Beta.Item  = Dog
}

// MARK: - Concrete disambiguation routes to the right per-protocol witness.

func concrete(a: (Bag as any Alpha).Item, b: (Bag as any Beta).Item) {
  wantCat(a) // ok: Bag's Alpha.Item is Cat
  wantDog(b) // ok: Bag's Beta.Item is Dog
  wantCat(b) // expected-error {{cannot convert value of type 'Bag.Item' (aka 'Dog') to expected argument type 'Cat'}}
}

// MARK: - Diagnostics for malformed qualifiers.

func badMember(x: (Bag as any Alpha).Nope) {} // expected-error {{protocol 'Alpha' has no associated type named 'Nope'}}

func notAProtocol(x: (Bag as Cat).Item) {} // expected-error {{must be a protocol}}

// MARK: - Abstract use collapses; we warn and point at the workaround.

func abstractCollapse<T: Alpha & Beta>(t: T, a: (T as any Alpha).Item) {
  // expected-warning@-1 {{disambiguation of associated type 'Item' has no effect}}
  _ = t
  _ = a
}

// MARK: - The split-parameter idiom keeps the two associated types distinct.

func split<T: Alpha, U: Beta>(t: T, u: U, a: T.Item, b: U.Item) {
  let x: T.Item = a // ok
  _ = x
}

func callSplit(bag: Bag) {
  split(t: bag, u: bag, a: Cat(), b: Dog()) // ok: T=Bag -> Cat, U=Bag -> Dog
  split(t: bag, u: bag, a: Dog(), b: Dog()) // expected-error {{cannot convert value of type 'Dog' to expected argument type 'Bag.Item' (aka 'Cat')}}
}
