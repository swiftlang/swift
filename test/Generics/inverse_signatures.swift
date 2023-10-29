// RUN: %target-swift-frontend -enable-experimental-feature NoncopyableGenerics -verify -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s --implicit-check-not "error:"

// REQUIRES: asserts

// CHECK-LABEL: (file).genericFn@
// CHECK: Generic signature: <T where T : Copyable>
func genericFn<T>(_ t: T) {}

// CHECK-LABEL: .withInverse@
// CHECK: Generic signature: <T>
func withInverse<T: ~Copyable>(_ t: borrowing T) {}

// CHECK-LABEL: .where1@
// CHECK: Generic signature: <T>
func where1<T>(_ t: borrowing T) where T: ~Copyable {}

// CHECK-LABEL: .where2@
// CHECK: Generic signature: <T where T : NoCopyP>
func where2<T>(_ t: borrowing T) where T: NoCopyP, T: ~Copyable {}

// CHECK-LABEL: .where3@
// CHECK: Generic signature: <T where T : CopyP>
func where3<T>(_ t: T) where T: CopyP, T: ~Copyable {}

// CHECK-LABEL: .where4@
// CHECK: Generic signature: <T where T : Equatable>
func where4<T>(_ t: borrowing T) where T: Equatable, T: ~Copyable {}

// CHECK-LABEL: .compose1@
// CHECK: Generic signature: <T where T : NoCopyP>
func compose1<T: NoCopyP & ~Copyable>(_ t: borrowing T) {}

// CHECK-LABEL: .compose2@
// CHECK: Generic signature: <T where T : CopyP>
func compose2<T: CopyP & ~Copyable>(_ t: T) {}

// CHECK-LABEL: .compose3@
// CHECK: Canonical generic signature: <τ_0_0 where τ_0_0 : CopyP>
func compose3(_ t: some CopyP & ~Copyable) {}

// CHECK-LABEL: .compose4@
// CHECK: Canonical generic signature: <τ_0_0 where τ_0_0 : NoCopyP>
func compose4(_ t: inout some NoCopyP & ~Copyable) {}

// CHECK-LABEL: .f1@
// CHECK: Generic signature: <T where T : Copyable, T : NoCopyP>
func f1<T: NoCopyP>(_ t: T) {}

// CHECK-LABEL: .withSome@
// CHECK: Canonical generic signature: <τ_0_0 where τ_0_0 : Copyable>
func withSome(_ t: some Any) {}

// CHECK-LABEL: .withSomeEquatable@
// CHECK: Canonical generic signature: <τ_0_0 where τ_0_0 : Copyable, τ_0_0 : Equatable>
func withSomeEquatable(_ t: some Equatable) {}

// CHECK-LABEL: .withSomeProto@
// CHECK: Canonical generic signature: <τ_0_0 where τ_0_0 : Copyable, τ_0_0 : NoCopyP>
func withSomeProto(_ t: some NoCopyP) {}

// CHECK-LABEL: .withInverseSome@
// CHECK: Canonical generic signature: <τ_0_0>
func withInverseSome(_ t: borrowing some ~Copyable) {}

// CHECK-LABEL: .S1@
// CHECK: Generic signature: <T where T : Copyable>
struct S1<T> {}

// CHECK-LABEL: .S1_I@
// CHECK: Generic signature: <T>
struct S1_I<T: ~Copyable> {}

// CHECK-LABEL: .C1@
// CHECK: Generic signature: <T, U where T : Copyable, U : Copyable>
class C1<T, U> {}

// CHECK-LABEL: .C1_IC@
// CHECK: Generic signature: <T, U where U : Copyable>
class C1_IC<T: ~Copyable, U> {}

// CHECK-LABEL: .C1_CI@
// CHECK: Generic signature: <T, U where T : Copyable>
class C1_CI<T, U> where U: ~Copyable {}

// CHECK-LABEL: .C1_II@
// CHECK: Generic signature: <T, U>
class C1_II<T: ~Copyable, U: ~Copyable> {}

// CHECK-LABEL: .NoCopyP@
// CHECK: Requirement signature: <Self>
protocol NoCopyP: ~Copyable {}

// CHECK-LABEL: .NoCopyP2@
// CHECK: Requirement signature: <Self where Self : NoCopyP>
protocol NoCopyP2 where Self: ~Copyable & NoCopyP {}

// CHECK-LABEL: .CopyP@
// CHECK: Requirement signature: <Self where Self : Copyable>
protocol CopyP {}

// CHECK-LABEL: .CopyP2@
// CHECK: Requirement signature: <Self where Self : CopyP>
protocol CopyP2: CopyP, ~Copyable {}  // expected-warning {{protocol 'CopyP2' should be declared to refine 'Copyable' due to a same-type constraint on 'Self'}}

// CHECK-LABEL: .CopyP2_Fixed@
// CHECK: Requirement signature: <Self where Self : CopyP>
protocol CopyP2_Fixed: CopyP {}

// CHECK-LABEL: .CopyInheritsNC@
// CHECK: Requirement signature: <Self where Self : Copyable, Self : NoCopyP>
protocol CopyInheritsNC: NoCopyP {}

// CHECK-LABEL: .P2@
// CHECK: <Self where Self : Copyable, Self.[P2]A : Copyable>
protocol P2 { associatedtype A }

// CHECK-LABEL: .P2_IC@
// CHECK: <Self where Self.[P2_IC]A : Copyable>
protocol P2_IC: ~Copyable { associatedtype A }

// CHECK-LABEL: .P2_CI@
// CHECK: Requirement signature: <Self where Self : Copyable>
protocol P2_CI { associatedtype A: ~Copyable }

// CHECK-LABEL: .P2_II@
// CHECK: Requirement signature: <Self>
protocol P2_II: ~Copyable { associatedtype A: ~Copyable }

// CHECK-LABEL: .Cond@
// CHECK: Generic signature: <T>
// CHECK-NEXT: Canonical generic signature: <τ_0_0>
struct Cond<T: ~Copyable>: ~Copyable {}

// CHECK-LABEL: ExtensionDecl line={{.*}} base=Cond
// CHECK: Generic signature: <T where T : Copyable>
// CHECK-NEXT: Canonical generic signature: <τ_0_0 where τ_0_0 : Copyable>

// CHECK-LABEL: ExtensionDecl line={{.*}} base=Cond
// CHECK:       (normal_conformance type="Cond<T>" protocol="Copyable"
// CHECK-NEXT:       (requirement "T" conforms_to "Copyable"))
extension Cond: Copyable where T: Copyable {}
