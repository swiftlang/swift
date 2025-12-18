// RUN: %target-swift-frontend \
// RUN:   -enable-experimental-feature SuppressedAssociatedTypesWithDefaults \
// RUN:   -verify -typecheck %s -debug-generic-signatures \
// RUN:   -debug-inverse-requirements 2>&1 | %FileCheck %s --implicit-check-not "error:"

// REQUIRES: swift_feature_SuppressedAssociatedTypesWithDefaults

// ----------
// In all of the Ord_* protocols, A IS NOT a primary associatedtype,
// so it DOES NOT get a default requirements in extensions or when
// constraining a generic parameter

// CHECK-LABEL: .Ord_CI@
// CHECK: Requirement signature: <Self where Self : Copyable, Self : Escapable, Self.[Ord_CI]A : Escapable>
protocol Ord_CI { associatedtype A: ~Copyable }

// CHECK-LABEL: .Ord_II@
// CHECK: Requirement signature: <Self where Self : Escapable, Self.[Ord_II]A : Escapable>
protocol Ord_II: ~Copyable { associatedtype A: ~Copyable }

// CHECK-LABEL: .Ord_IIII@
// CHECK: Requirement signature: <Self>
protocol Ord_IIII: ~Copyable, ~Escapable { associatedtype A: ~Copyable, ~Escapable }

// CHECK-LABEL: ExtensionDecl line={{.*}} base=Ord_CI
// CHECK-NEXT: Generic signature: <Self where Self : Ord_CI>
extension Ord_CI {}

// CHECK-LABEL: ExtensionDecl line={{.*}} base=Ord_II
// CHECK-NEXT: Generic signature: <Self where Self : Copyable, Self : Ord_II>
extension Ord_II {}

// CHECK-LABEL: ExtensionDecl line={{.*}} base=Ord_IIII
// CHECK-NEXT: Generic signature: <Self where Self : Copyable, Self : Escapable, Self : Ord_IIII>
extension Ord_IIII {}

// CHECK-LABEL: .test1(_:y:z:)@
// CHECK-NEXT: Generic signature: <X, Y, Z where X : Ord_CI, Y : Copyable, Y : Ord_II,
// CHECK-SAME:                                   Z : Copyable, Z : Escapable, Z : Ord_IIII>
func test1<X, Y, Z>(_ x: X, y: Y, z: Z) where
  X: Ord_CI, Y: Ord_II, Z: Ord_IIII {}



// ----------
// In all of the Pri_* protocols, A IS a primary associatedtype,
// so it DOES get a default requirements in extensions or when
// constraining a generic parameter

// CHECK-LABEL: .Pri_CI@
// CHECK: Requirement signature: <Self where Self : Copyable, Self : Escapable, Self.[Pri_CI]A : Escapable>
protocol Pri_CI<A> { associatedtype A: ~Copyable }

// CHECK-LABEL: .Pri_II@
// CHECK: Requirement signature: <Self where Self : Escapable, Self.[Pri_II]A : Escapable>
protocol Pri_II<A>: ~Copyable { associatedtype A: ~Copyable }

// CHECK-LABEL: .Pri_IIII@
// CHECK: Requirement signature: <Self>
protocol Pri_IIII<A>: ~Copyable, ~Escapable { associatedtype A: ~Copyable, ~Escapable }

// CHECK-LABEL: ExtensionDecl line={{.*}} base=Pri_CI
// CHECK-NEXT: Generic signature: <Self where Self : Pri_CI, Self.[Pri_CI]A : Copyable>
extension Pri_CI {}

// CHECK-LABEL: ExtensionDecl line={{.*}} base=Pri_II
// CHECK-NEXT: Generic signature: <Self where Self : Copyable, Self : Pri_II, Self.[Pri_II]A : Copyable>
extension Pri_II {}

// CHECK-LABEL: ExtensionDecl line={{.*}} base=Pri_IIII
// CHECK-NEXT: Generic signature: <Self where Self : Copyable, Self : Escapable, Self : Pri_IIII,
// CHECK-SAME:                                Self.[Pri_IIII]A : Copyable, Self.[Pri_IIII]A : Escapable>
extension Pri_IIII {}

// CHECK-LABEL: .test2(_:y:z:)@
// CHECK-NEXT: Generic signature: <X, Y, Z where X : Pri_CI, Y : Copyable, Y : Pri_II,
// CHECK-SAME: Z : Copyable, Z : Escapable, Z : Pri_IIII,
// CHECK-SAME: X.[Pri_CI]A : Copyable, Y.[Pri_II]A : Copyable, Z.[Pri_IIII]A : Copyable, Z.[Pri_IIII]A : Escapable>
func test2<X, Y, Z>(_ x: X, y: Y, z: Z) where
  X: Pri_CI, Y: Pri_II, Z: Pri_IIII {}

struct RequireCopy<X: Copyable> {}

// CHECK-LABEL: .ImplyP@
// CHECK-NEXT: Generic signature: <V where V : Pri_CI, V.[Pri_CI]A : Copyable>
struct ImplyP<V> where V: Pri_CI {}

// CHECK-LABEL: .implied1@
// CHECK-NEXT: Generic signature: <T where T : Pri_CI, T.[Pri_CI]A : Copyable>
func implied1<T>(_ t: ImplyP<T>) {}

// CHECK-LABEL: .P3@
// CHECK: Requirement signature: <Self where Self.[P3]B : Copyable>
protocol P3 where Self: (~Copyable & ~Escapable) { associatedtype B: ~Escapable }

// CHECK-LABEL: .P4@
// CHECK: Requirement signature: <Self where Self : Copyable, Self.[P4]B : Copyable, Self.[P4]C : Escapable>
protocol P4<B, D>: ~Escapable {
  associatedtype B: ~Escapable
  associatedtype C: ~Copyable
  associatedtype D: ~Escapable, ~Copyable
}

// CHECK-LABEL: .test3@
// CHECK-NEXT: Generic signature: <T where T : Escapable, T : P4,
// CHECK-SAME:                             T.[P4]B : Escapable, T.[P4]D : Copyable, T.[P4]D : Escapable>
func test3<T>(_ p: T) where T: P4 {}


// CHECK-LABEL: .View@
// CHECK-NEXT: Requirement signature: <Self where Self : Escapable,
// CHECK-SAME:                                    Self.[View]Body : View,
// CHECK-SAME:                                    Self.[View]Body.[View]Body : Copyable>
protocol View<Body>: ~Copyable {
  associatedtype Body: ~Copyable, View
}

// CHECK-LABEL: ExtensionDecl line={{.*}} base=View
// CHECK-NEXT: Generic signature: <Self where Self : Copyable, Self : View, Self.[View]Body : Copyable>
extension View {}

// CHECK-LABEL: .test4_1@
// CHECK-NEXT: Generic signature: <V where V : Copyable, V : View, V.[View]Body : Copyable>
func test4_1<V: View>(_ v: V) {}

// CHECK-LABEL: .test4_2@
// CHECK:  Canonical generic signature: <τ_0_0 where τ_0_0 : Copyable, τ_0_0 : View, τ_0_0.[View]Body : Copyable>
func test4_2(_ v: some View) {}


// CHECK-LABEL: .View2@
// CHECK-NEXT: Requirement signature: <Self where Self : Escapable,
// CHECK-SAME:                                    Self.[View2]Body : View2>
protocol View2<Body>: ~Copyable {
  associatedtype Body: ~Copyable, View2 where Body.Body: ~Copyable
}

// CHECK-LABEL: ExtensionDecl line={{.*}} base=View2
// CHECK-NEXT: Generic signature: <Self where Self : Copyable, Self : View2, Self.[View2]Body : Copyable>
extension View2 {}

// CHECK-LABEL: .test5_1@
// CHECK-NEXT: Generic signature: <V where V : Copyable, V : View2, V.[View2]Body : Copyable>
func test5_1<V: View2>(_ v: V) {}

// CHECK-LABEL: .test5_2@
// CHECK:  Canonical generic signature: <τ_0_0 where τ_0_0 : Copyable, τ_0_0 : View2, τ_0_0.[View2]Body : Copyable>
func test5_2(_ v: some View2) {}

// CHECK-LABEL: .Red@
// CHECK-NEXT: Requirement signature: <Self where Self : Escapable,
// CHECK-SAME:                                    Self.[Red]R : Blue,
// CHECK-SAME:                                    Self.[Red]R.[Blue]B : Copyable
protocol Red<R>: ~Copyable {
  associatedtype R: Blue, ~Copyable
}

// CHECK-LABEL: .Blue@
// CHECK-NEXT: Requirement signature: <Self where Self : Escapable,
// CHECK-SAME:                                    Self.[Blue]B : Red,
// CHECK-SAME:                                    Self.[Blue]B.[Red]R : Copyable>
protocol Blue<B>: ~Copyable {
  associatedtype B: Red, ~Copyable
}

// CHECK-LABEL: .test6@
// CHECK: Canonical generic signature: <τ_0_0 where τ_0_0 : Copyable, τ_0_0 : Red, τ_0_0.[Red]R : Copyable>
func test6(_: some Red) {}


// CHECK-LABEL: .Base@
// CHECK-NEXT: Requirement signature: <Self where Self : Escapable, Self.[Base]Elm : Escapable, Self.[Base]Iter : Escapable>
protocol Base<Elm>: ~Copyable {
  associatedtype Elm: ~Copyable
  associatedtype Iter: ~Copyable
}

// CHECK-LABEL: .Derived1@
// CHECK-NEXT: Requirement signature: <Self where Self : Copyable, Self : Base, Self.[Base]Elm : Copyable>
protocol Derived1<Elm>: Base {}

// CHECK-LABEL: .Derived2@
// CHECK-NEXT: Requirement signature: <Self where Self : Base, Self.[Base]Elm : Copyable>
protocol Derived2<Elm>: Base, ~Copyable {}

// CHECK-LABEL: .Derived3@
// CHECK-NEXT: Requirement signature: <Self where Self : Copyable, Self : Base>
protocol Derived3<Elm>: Base where Elm: ~Copyable {}

// CHECK-LABEL: .Derived4@
// CHECK-NEXT: Requirement signature: <Self where Self : Base>
protocol Derived4<Elm>: Base, ~Copyable where Elm: ~Copyable {}

// CHECK-LABEL: .Derived5@
// CHECK-NEXT: Requirement signature: <Self where Self : Copyable, Self : Base, Self.[Base]Elm : Copyable>
protocol Derived5: Base {}

// CHECK-LABEL: .Derived6@
// CHECK-NEXT: Requirement signature: <Self where Self : Copyable, Self : Base, Self.[Base]Elm : Copyable>
protocol Derived6<Iter>: Base {}

// CHECK-LABEL: .SecondOrder@
// CHECK-NEXT: Requirement signature: <Self where Self : Derived6, Self.[Base]Iter : Copyable>
protocol SecondOrder: Derived6 {}

// CHECK-LABEL: .SecondOrderSupp1@
// CHECK-NEXT: Requirement signature: <Self where Self : Derived6>
protocol SecondOrderSupp1: Derived6 where Self.Iter: ~Copyable {}

// CHECK-LABEL: .Derived7@
// CHECK-NEXT: Requirement signature: <Self where Self : Copyable, Self : Base, Self.[Base]Elm : Copyable, Self.[Base]Iter : Copyable>
protocol Derived7: Base {
  associatedtype Iter
}


// CHECK-LABEL: .SameType@
// CHECK-NEXT: Requirement signature: <Self where Self : Copyable, Self : Escapable,
// CHECK-SAME:                                    Self.[SameType]A : Escapable,
// CHECK-SAME:                                    Self.[SameType]A == Self.[SameType]B>
protocol SameType<A> {
  associatedtype A: ~Copyable
  associatedtype B: ~Copyable where A == B
}
// CHECK-LABEL: .test8@
// CHECK-NEXT: Generic signature: <T where T : SameType, T.[SameType]A : Copyable>
func test8<T: SameType>(_: T) {}


protocol Left {
  associatedtype A: ~Copyable
}
protocol Right {
  associatedtype A
}

// CHECK-LABEL: .testClash@
// CHECK-NEXT: Generic signature: <T where T : Left, T : Right>
func testClash<T>(_ t: T, _ a: T.A) where T: Left, T: Right {}
// T.A becomes Copyable


protocol Ping<A>: ~Copyable {
  associatedtype A: ~Copyable
  associatedtype B: ~Copyable
}

// CHECK-LABEL: .testExpansion1@
// CHECK: Generic signature: <T where T : Copyable, T : Ping,
// CHECK-SAME:                T.[Ping]A : Copyable, T.[Ping]A : Ping,
// CHECK-SAME:                T.[Ping]A.[Ping]A : Copyable, T.[Ping]A.[Ping]A : Ping,
// CHECK-SAME:                T.[Ping]A.[Ping]A.[Ping]A : Copyable>
func testExpansion1<T>(_ t: T, _ a: T.A, _ aa: T.A.A, _ aaa: T.A.A.A)
  where T: Ping, T.A: Ping, T.A.A: Ping
{}

// CHECK-LABEL: .testExpansion2@
// CHECK: Generic signature: <T where T : Ping, T.[Ping]A : Ping, T.[Ping]B : Ping,
// CHECK-SAME:                T.[Ping]A.[Ping]A : Ping,
// CHECK-SAME:                T.[Ping]B.[Ping]A : Copyable, T.[Ping]B.[Ping]B : Ping,
// CHECK-SAME:                T.[Ping]A.[Ping]A.[Ping]A : Copyable
// CHECK-SAME:                T.[Ping]B.[Ping]B.[Ping]A : Copyable>
func testExpansion2<T>(_ t: borrowing T) where
  T: ~Copyable & Ping, T.A: ~Copyable & Ping, T.A.A: ~Copyable & Ping,
  T.B: Ping, T.B.B: Ping {}


// CHECK-LABEL: .Iterable@
// CHECK-NEXT: Requirement signature: <Self where Self : Escapable, Self.[Iterable]Element : Escapable>
protocol Iterable<Element>: ~Copyable {
  associatedtype Element: ~Copyable
}

// CHECK-LABEL: .PersistedDictionary@
// CHECK-NEXT: Requirement signature: <Self where Self : Iterable, Self.[Iterable]Element == Self.[PersistedDictionary]Value, Self.[PersistedDictionary]Key : Escapable, Self.[PersistedDictionary]Strategy : Escapable>
protocol PersistedDictionary<Key, Value>: ~Copyable, Iterable<Self.Value> {
  associatedtype Key: ~Copyable
  associatedtype Value: ~Copyable
  associatedtype Strategy: ~Copyable
}

// CHECK-LABEL: ExtensionDecl line={{.*}} base=PersistedDictionary
// CHECK-NEXT: Generic signature: <Self where Self : Copyable, Self : PersistedDictionary, Self.[Iterable]Element : Copyable, Self.[PersistedDictionary]Key : Copyable>
extension PersistedDictionary {}
