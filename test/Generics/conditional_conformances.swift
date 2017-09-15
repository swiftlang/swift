// RUN: %target-typecheck-verify-swift -typecheck %s -verify
// RUN: %target-typecheck-verify-swift -typecheck -debug-generic-signatures %s > %t.dump 2>&1
// RUN: %FileCheck %s < %t.dump

protocol P1 {}
protocol P2 {}
protocol P3 {}
protocol P4: P1 {}

struct Free<T> {}
// CHECK-LABEL: ExtensionDecl line={{.*}} base=Free<T>
// CHECK-NEXT: (normal_conformance type=Free<T> protocol=P2
// CHECK-NEXT:   conforms_to: τ_0_0 P1)
extension Free: P2 where T: P1 {}


struct Constrained<T: P1> {}
// CHECK-LABEL: ExtensionDecl line={{.*}} base=Constrained<T>
// CHECK-NEXT: (normal_conformance type=Constrained<T> protocol=P2
// CHECK-NEXT:   conforms_to: τ_0_0 P3)
extension Constrained: P2 where T: P3 {}


struct RedundantSame<T: P1> {}
// CHECK-LABEL: ExtensionDecl line={{.*}} base=RedundantSame<T>
// CHECK-NEXT: (normal_conformance type=RedundantSame<T> protocol=P2)
extension RedundantSame: P2 where T: P1 {}

struct RedundantSuper<T: P4> {}
// CHECK-LABEL: ExtensionDecl line={{.*}} base=RedundantSuper<T>
// CHECK-NEXT: (normal_conformance type=RedundantSuper<T> protocol=P2)
extension RedundantSuper: P2 where T: P1 {}

struct OverlappingSub<T: P1> {}
// CHECK-LABEL: ExtensionDecl line={{.*}} base=OverlappingSub<T>
// CHECK-NEXT: (normal_conformance type=OverlappingSub<T> protocol=P2
// CHECK-NEXT:   conforms_to: τ_0_0 P4)
extension OverlappingSub: P2 where T: P4 {}


struct SameType<T> {}
// CHECK-LABEL: ExtensionDecl line={{.*}} base=SameType<Int>
// CHECK-NEXT: (normal_conformance type=SameType<Int> protocol=P2
// CHECK-NEXT:   same_type: τ_0_0 Int)
extension SameType: P2 where T == Int {}


struct SameTypeGeneric<T, U> {}
// CHECK-LABEL: ExtensionDecl line={{.*}} base=SameTypeGeneric<T, T>
// CHECK-NEXT: (normal_conformance type=SameTypeGeneric<T, T> protocol=P2
// CHECK-NEXT:   same_type: τ_0_0 τ_0_1)
extension SameTypeGeneric: P2 where T == U {}


struct Infer<T, U> {}
// CHECK-LABEL: ExtensionDecl line={{.*}} base=Infer<Constrained<U>, U>
// CHECK-NEXT: (normal_conformance type=Infer<Constrained<U>, U> protocol=P2
// CHECK-NEXT:   same_type: τ_0_0 Constrained<τ_0_1>
// CHECK-NEXT:   conforms_to:  τ_0_1 P1)
extension Infer: P2 where T == Constrained<U> {}


struct InferRedundant<T, U: P1> {}
// CHECK-LABEL: ExtensionDecl line={{.*}} base=InferRedundant<Constrained<U>, U>
// CHECK-NEXT: (normal_conformance type=InferRedundant<Constrained<U>, U> protocol=P2
// CHECK-NEXT:   same_type: τ_0_0 Constrained<τ_0_1>)
extension InferRedundant: P2 where T == Constrained<U> {}


class C1 {}
class C2: C1 {}
class C3: C2 {}

struct ClassFree<T> {}
// CHECK-LABEL: ExtensionDecl line={{.*}} base=ClassFree<T>
// CHECK-NEXT: (normal_conformance type=ClassFree<T> protocol=P2
// CHECK-NEXT:   superclass: τ_0_0 C1)
extension ClassFree: P2 where T: C1 {}


struct ClassMoreSpecific<T: C1> {}
// CHECK-LABEL: ExtensionDecl line={{.*}} base=ClassMoreSpecific<T>
// CHECK-NEXT: (normal_conformance type=ClassMoreSpecific<T> protocol=P2
// CHECK-NEXT:   superclass: τ_0_0 C3)
extension ClassMoreSpecific: P2 where T: C3 {}


struct ClassLessSpecific<T: C3> {}
// CHECK-LABEL: ExtensionDecl line={{.*}} base=ClassLessSpecific<T>
// CHECK-NEXT: (normal_conformance type=ClassLessSpecific<T> protocol=P2)
extension ClassLessSpecific: P2 where T: C1 {}
