// RUN: %target-typecheck-verify-swift \
// RUN:   -enable-experimental-feature AssociatedTypeDisambiguation \
// RUN:   -disable-experimental-parser-round-trip

// REQUIRES: swift_feature_AssociatedTypeDisambiguation

// Stress cases: more than two protocols, multiple associated types,
// inheritance, generic conforming types, and multi-way disambiguation.

struct X {}
struct Y {}
struct Z {}
struct W {}

func wantX(_: X) {}
func wantY(_: Y) {}
func wantZ(_: Z) {}
func wantW(_: W) {}

// MARK: - Three protocols sharing one associated type name.

protocol A { associatedtype Item }
protocol B { associatedtype Item }
protocol C { associatedtype Item }

struct Tri: A, B, C {
  typealias A.Item = X
  typealias B.Item = Y
  typealias C.Item = Z
}

func tri(a: (Tri as any A).Item, b: (Tri as any B).Item, c: (Tri as any C).Item) {
  wantX(a)
  wantY(b)
  wantZ(c)
  wantX(c) // expected-error {{cannot convert value of type 'Tri.Item' (aka 'Z') to expected argument type 'X'}}
}

// MARK: - Multiple associated types per protocol: a shared name and a unique one.

protocol P1 {
  associatedtype Item
  associatedtype Extra
}
protocol P2 {
  associatedtype Item
}

struct Mix: P1, P2 {
  typealias P1.Item = X
  typealias P2.Item = Y
  typealias Extra = Z // ordinary, non-conflicting
}

func mix(a: (Mix as any P1).Item, b: (Mix as any P2).Item, e: Mix.Extra) {
  wantX(a)
  wantY(b)
  wantZ(e)
}

// MARK: - Protocol inheritance: the inherited requirement is qualified by its
// declaring protocol.

protocol Base { associatedtype Item }
protocol Refined: Base {}

struct Inh: Refined {
  typealias Base.Item = W
}

func inh(a: (Inh as any Base).Item) {
  wantW(a)
}

// MARK: - Generic conforming type with disambiguated witnesses.

struct GBag<E>: A, B {
  typealias A.Item = E
  typealias B.Item = Array<E>
}

func gbag(a: (GBag<X> as any A).Item, b: (GBag<X> as any B).Item) {
  wantX(a)
  let arr: [X] = b
  _ = arr
}

// MARK: - Multi-way split-parameter idiom (three protocols, three parameters).

func split3<U: A, V: B, T: C>(u: U, v: V, t: T,
                              a: U.Item, b: V.Item, c: T.Item) {}

func callSplit3(tri: Tri) {
  split3(u: tri, v: tri, t: tri, a: X(), b: Y(), c: Z()) // ok
  split3(u: tri, v: tri, t: tri, a: Y(), b: Y(), c: Z()) // expected-error {{cannot convert value of type 'Y' to expected argument type 'Tri.Item' (aka 'X')}}
}
