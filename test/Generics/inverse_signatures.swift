// RUN: %target-swift-frontend \
// RUN:   -enable-experimental-feature SuppressedAssociatedTypes \
// RUN:   -verify -typecheck %s -debug-generic-signatures \
// RUN:   -debug-inverse-requirements 2>&1 | %FileCheck %s --implicit-check-not "error:"

// REQUIRES: swift_feature_SuppressedAssociatedTypes

// CHECK-LABEL: (file).genericFn@
// CHECK: Generic signature: <T where T : Copyable, T : Escapable>
func genericFn<T>(_ t: T) {}

// CHECK-LABEL: .withInverse1@
// CHECK: Generic signature: <T where T : Escapable>
func withInverse1<T: ~Copyable>(_ t: borrowing T) {}

// CHECK-LABEL: .withInverse2@
// CHECK: Generic signature: <T where T : Copyable>
func withInverse2<T: ~Escapable>(_ t: borrowing T) {}

// CHECK-LABEL: .withInverse3@
// CHECK: Generic signature: <T>
func withInverse3<T: ~Copyable & ~Escapable>(_ t: borrowing T) {}

// CHECK-LABEL: .where1@
// CHECK: Generic signature: <T where T : Escapable>
func where1<T>(_ t: borrowing T) where T: ~Copyable {}

// CHECK-LABEL: .where2@
// CHECK: Generic signature: <T where T : NoCopyP>
func where2<T>(_ t: borrowing T) where T: NoCopyP, T: ~Copyable {}

// CHECK-LABEL: .where3@
// CHECK: Generic signature: <T where T : Escapable, T : Empty>
func where3<T>(_ t: borrowing T) where T: Empty, T: ~Copyable {}

// CHECK-LABEL: .where4@
// CHECK: Generic signature: <T where T : Copyable>
func where4<T>(_ t: borrowing T) where T: ~Escapable {}

// CHECK-LABEL: .where5@
// CHECK: Generic signature: <T where T : Copyable, T : Empty>
func where5<T>(_ t: borrowing T) where T: Empty, T: ~Escapable {}

// CHECK-LABEL: .where6@
// CHECK: Generic signature: <T where T : Escapable, T : Empty>
func where6<T>(_ t: borrowing T) where T: Empty, T: ~Copyable {}

// CHECK-LABEL: .compose1@
// CHECK: Generic signature: <T where T : NoCopyP>
func compose1<T: NoCopyP & ~Copyable>(_ t: borrowing T) {}

// CHECK-LABEL: .compose3@
// CHECK: Canonical generic signature: <τ_0_0 where τ_0_0 : NoCopyP>
func compose3(_ t: inout some NoCopyP & ~Copyable) {}

// CHECK-LABEL: .f1@
// CHECK: Generic signature: <T where T : Copyable, T : NoCopyP>
func f1<T: NoCopyP>(_ t: T) {}

// CHECK-LABEL: .withSome@
// CHECK: Canonical generic signature: <τ_0_0 where τ_0_0 : Copyable, τ_0_0 : Escapable>
func withSome(_ t: some Any) {}

// CHECK-LABEL: .withSomeEmpty@
// CHECK: Canonical generic signature: <τ_0_0 where τ_0_0 : Copyable, τ_0_0 : Escapable, τ_0_0 : Empty>
func withSomeEmpty(_ t: some Empty) {}

// CHECK-LABEL: .withSomeProto@
// CHECK: Canonical generic signature: <τ_0_0 where τ_0_0 : Copyable, τ_0_0 : NoCopyP>
func withSomeProto(_ t: some NoCopyP) {}

// CHECK-LABEL: .withInverseSome@
// CHECK: Canonical generic signature: <τ_0_0 where τ_0_0 : Escapable>
func withInverseSome(_ t: borrowing some ~Copyable) {}

// CHECK-LABEL: .checkAnyObject@
// CHECK: Canonical generic signature: <τ_0_0 where τ_0_0 : AnyObject, τ_0_0 : Copyable, τ_0_0 : Escapable>
func checkAnyObject<Result>(_ t: Result) where Result: AnyObject {}

// CHECK-LABEL: .checkSoup@
// CHECK: Canonical generic signature: <τ_0_0 where τ_0_0 : Soup>
class Soup {}
func checkSoup<T>(_ t: T) where T: Soup {}

// CHECK-LABEL: .S1@
// CHECK: Generic signature: <T where T : Copyable, T : Escapable>
struct S1<T> {}

// CHECK-LABEL: .S1_I@
// CHECK: Generic signature: <T where T : Escapable>
struct S1_I<T: ~Copyable> {}

// CHECK-LABEL: .C1@
// CHECK: Generic signature: <T, U where T : Copyable, T : Escapable, U : Copyable, U : Escapable>
class C1<T, U> {}

// CHECK-LABEL: .C1_IC@
// CHECK: Generic signature: <T, U where T : Escapable, U : Copyable, U : Escapable>
class C1_IC<T: ~Copyable, U> {}

// CHECK-LABEL: .C1_CI@
// CHECK: Generic signature: <T, U where T : Copyable, T : Escapable, U : Escapable>
class C1_CI<T, U> where U: ~Copyable {}

// CHECK-LABEL: .C1_II@
// CHECK: Generic signature: <T, U where T : Escapable, U : Escapable>
class C1_II<T: ~Copyable, U: ~Copyable> {}

// CHECK-LABEL: .Empty@
// CHECK: Requirement signature: <Self>
protocol Empty: ~Copyable, ~Escapable {}

// CHECK-LABEL: .NoEscapeP@
// CHECK: Requirement signature: <Self where Self : Copyable>
protocol NoEscapeP: ~Escapable {}

// CHECK-LABEL: .NoEscapeP2@
// CHECK: Requirement signature: <Self where Self : NoEscapeP>
protocol NoEscapeP2 where Self: NoEscapeP & ~Escapable {}

// CHECK-LABEL: .ForgotTildeEscape@
// CHECK: Requirement signature: <Self where Self : Escapable, Self : NoEscapeP>
protocol ForgotTildeEscape where Self: NoEscapeP {}

// CHECK-LABEL: .NoCopyP@
// CHECK: Requirement signature: <Self where Self : Escapable>
protocol NoCopyP: ~Copyable {}

// CHECK-LABEL: .NoCopyP2@
// CHECK: Requirement signature: <Self where Self : NoCopyP>
protocol NoCopyP2 where Self: ~Copyable & NoCopyP {}

// CHECK-LABEL: .CopyP@
// CHECK: Requirement signature: <Self where Self : Copyable, Self : Escapable>
protocol CopyP {}

// CHECK-LABEL: .CopyP2@
// CHECK: Requirement signature: <Self where Self : CopyP>
protocol CopyP2: CopyP {}

// CHECK-LABEL: .CopyInheritsNC@
// CHECK: Requirement signature: <Self where Self : Copyable, Self : NoCopyP>
protocol CopyInheritsNC: NoCopyP {}

// CHECK-LABEL: .P2@
// CHECK: <Self where Self : Copyable, Self : Escapable, Self.[P2]A : Copyable, Self.[P2]A : Escapable>
protocol P2 { associatedtype A }

// CHECK-LABEL: .P2_IC@
// CHECK: <Self where Self : Escapable, Self.[P2_IC]A : Copyable, Self.[P2_IC]A : Escapable>
protocol P2_IC: ~Copyable { associatedtype A }

// CHECK-LABEL: .P2_CI@
// CHECK: Requirement signature: <Self where Self : Copyable, Self : Escapable, Self.[P2_CI]A : Escapable>
protocol P2_CI { associatedtype A: ~Copyable }

// CHECK-LABEL: .P2_II@
// CHECK: Requirement signature: <Self where Self : Escapable, Self.[P2_II]A : Escapable>
protocol P2_II: ~Copyable { associatedtype A: ~Copyable }

// CHECK-LABEL: .P3@
// CHECK: Requirement signature: <Self where Self.[P3]B : Copyable>
protocol P3 where Self: (~Copyable & ~Escapable) { associatedtype B: ~Escapable }

// CHECK-LABEL: .P4@
// CHECK: Requirement signature: <Self where Self : Copyable, Self.[P4]B : Copyable, Self.[P4]C : Escapable>
protocol P4: ~Escapable {
  associatedtype B: ~Escapable
  associatedtype C: ~Copyable
  associatedtype D: ~Escapable, ~Copyable
}

// CHECK-LABEL: .Explicit@
// CHECK: Requirement signature: <Self where Self : Copyable, Self : Escapable, Self.[Explicit]Elm : Copyable, Self.[Explicit]Elm : Escapable>
protocol Explicit: Copyable, Escapable {
  associatedtype Elm: Copyable, Escapable
}

// CHECK-LABEL: .Cond@
// CHECK: Generic signature: <T where T : Escapable>
// CHECK-NEXT: Canonical generic signature: <τ_0_0 where τ_0_0 : Escapable>
struct Cond<T: ~Copyable>: ~Copyable {}

// CHECK-LABEL: ExtensionDecl line={{.*}} base=Cond
// CHECK: <T where T : Copyable, T : Escapable>
// CHECK-NEXT: Canonical generic signature: <τ_0_0 where τ_0_0 : Copyable, τ_0_0 : Escapable>

// CHECK-LABEL: ExtensionDecl line={{.*}} base=Cond
// CHECK:       (normal_conformance type="Cond<T>" protocol="Copyable"
// CHECK-NEXT:       (requirement "T" conforms_to "Copyable"))
extension Cond: Copyable where T: Copyable {}


// CHECK-LABEL: .FullyGenericArg@
// CHECK: Generic signature: <T>
// CHECK-NEXT: Canonical generic signature: <τ_0_0>
struct FullyGenericArg<T: ~Escapable & ~Copyable> {}

// CHECK-LABEL: StructDecl name=FullyGenericArg
// CHECK-NEXT:    (builtin_conformance type="FullyGenericArg<T>" protocol="Copyable"{{.*}})
// CHECK-NEXT:    (builtin_conformance type="FullyGenericArg<T>" protocol="Escapable"{{.*}})

// CHECK-LABEL: ExtensionDecl line={{.*}} base=FullyGenericArg
// CHECK: Generic signature: <T>
// CHECK-NEXT: Canonical generic signature: <τ_0_0>

// CHECK-LABEL: ExtensionDecl line={{.*}} base=FullyGenericArg
// CHECK-NEXT: (normal_conformance type="FullyGenericArg<T>" protocol="Empty"{{.*}})
extension FullyGenericArg: Empty where T: ~Copyable, T: ~Escapable {}
