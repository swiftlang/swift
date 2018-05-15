// RUN: %target-swift-frontend -typecheck -debug-generic-signatures %s 2>&1 | %FileCheck %s

// CHECK: Generic signature: <Self where Self : P1>
// CHECK-NEXT: Canonical generic signature: <τ_0_0 where τ_0_0 : P1>
// CHECK-LABEL: main.(file).P1@
// CHECK: Requirement signature: <Self>
// CHECK-NEXT: Canonical requirement signature: <τ_0_0>
protocol P1 {
    associatedtype A
    func f() -> A
}

// Recursion, and where clauses.
// CHECK: Generic signature: <Self where Self : P2>
// CHECK-NEXT: Canonical generic signature: <τ_0_0 where τ_0_0 : P2>
// CHECK-LABEL: main.(file).P2@
// CHECK: Requirement signature: <Self where Self.A : P2, Self.B : P2, Self.A.A == Self.B.A>
// CHECK-NEXT: Canonical requirement signature: <τ_0_0 where τ_0_0.A : P2, τ_0_0.B : P2, τ_0_0.A.A == τ_0_0.B.A>
protocol P2 {
    associatedtype A: P2
    associatedtype B: P2 where Self.A.A == Self.B.A
}

// CHECK-LABEL: StructDecl name=Basic
// CHECK: (normal_conformance type=Basic protocol=P1
// CHECK-NEXT: (assoc_type req=A type=Basic.A)
// CHECK-NEXT: (value req=f() witness=main.(file).Basic.f()@{{.*}}))
struct Basic: P1 {
    typealias A = Int
    func f() -> Int { fatalError() }
}

// Recursive conformances should have finite output.

// CHECK-LABEL: StructDecl name=Recur
// CHECK-NEXT: (normal_conformance type=Recur protocol=P2
// CHECK-NEXT:   (assoc_type req=A type=Recur.A)
// CHECK-NEXT:   (assoc_type req=B type=Recur.B)
// CHECK-NEXT:   (normal_conformance type=Recur protocol=P2 (details printed above))
// CHECK-NEXT:   (normal_conformance type=Recur protocol=P2 (details printed above)))
struct Recur: P2 {
    typealias A = Recur
    typealias B = Recur
}

// The full information about a conformance doesn't need to be printed twice.

// CHECK-LABEL: StructDecl name=NonRecur
// CHECK-NEXT: (normal_conformance type=NonRecur protocol=P2
// CHECK-NEXT:   (assoc_type req=A type=NonRecur.A)
// CHECK-NEXT:   (assoc_type req=B type=NonRecur.B)
// CHECK-NEXT:   (normal_conformance type=Recur protocol=P2
// CHECK-NEXT:     (assoc_type req=A type=Recur.A)
// CHECK-NEXT:     (assoc_type req=B type=Recur.B)
// CHECK-NEXT:     (normal_conformance type=Recur protocol=P2 (details printed above))
// CHECK-NEXT:     (normal_conformance type=Recur protocol=P2 (details printed above)))
// CHECK-NEXT:   (normal_conformance type=Recur protocol=P2 (details printed above)))
struct NonRecur: P2 {
    typealias A = Recur
    typealias B = Recur
}

// Conditional conformance.

struct Generic<T> {}
// CHECK-LABEL: ExtensionDecl line={{.*}} base=Generic<T>
// CHECK-NEXT: (normal_conformance type=Generic<T> protocol=P1
// CHECK-NEXT:   (assoc_type req=A type=Generic<T>.A)
// CHECK-NEXT:   (value req=f() witness=main.(file).Generic.f()@{{.*}})
// CHECK-NEXT:   conforms_to: T P1)
extension Generic: P1 where T: P1 {
    typealias A = T
    func f() -> T { fatalError() }
}


// Satisfying associated types with requirements with generic params
class Super<T> {}
// CHECK-LABEL: ExtensionDecl line={{.*}} base=Super<T>
// CHECK-NEXT: (normal_conformance type=Super<T> protocol=P2
// CHECK-NEXT:   (assoc_type req=A type=Super<T>.A)
// CHECK-NEXT:   (assoc_type req=B type=Super<T>.B)
// CHECK-NEXT:   (abstract_conformance protocol=P2)
// CHECK:        (abstract_conformance protocol=P2)
// CHECK:        conforms_to: T P2)
extension Super: P2 where T: P2 {
    typealias A = T
    typealias B = T
}

// Inherited/specialized conformances.
// CHECK-LABEL: ClassDecl name=Sub
// CHECK-NEXT: (inherited_conformance type=Sub protocol=P2
// CHECK-NEXT:   (specialized_conformance type=Super<Recur> protocol=P2
// CHECK-NEXT: Generic signature: <T where T : P2>
// CHECK-NEXT: Substitutions:
// CHECK-NEXT:   T -> Recur
// CHECK:      Conformance map:
// CHECK-NEXT:   T -> (normal_conformance type=Recur protocol=P2
// CHECK-NEXT:   (assoc_type req=A type=Recur.A)
// CHECK-NEXT:   (assoc_type req=B type=Recur.B)
// CHECK-NEXT:   (normal_conformance type=Recur protocol=P2 (details printed above))
// CHECK-NEXT:   (normal_conformance type=Recur protocol=P2 (details printed above)))
// CHECK-NEXT:     conforms_to: Recur P2
// CHECK-NEXT:     (normal_conformance type=Super<T> protocol=P2
// CHECK-NEXT:       (assoc_type req=A type=Super<T>.A)
// CHECK-NEXT:       (assoc_type req=B type=Super<T>.B)
// CHECK-NEXT:       (abstract_conformance protocol=P2)
// CHECK:            (abstract_conformance protocol=P2)
// CHECK:            conforms_to: T P2)))
class Sub: Super<Recur> {}
