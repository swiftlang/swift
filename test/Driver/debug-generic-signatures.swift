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

// Simpler recursion
// CHECK: Generic signature: <Self where Self : P3>
// CHECK-NEXT: Canonical generic signature: <τ_0_0 where τ_0_0 : P3>
// CHECK-LABEL: main.(file).P3@
// CHECK: Requirement signature: <Self where Self.A : P3>
// CHECK-NEXT: Canonical requirement signature: <τ_0_0 where τ_0_0.A : P3>
protocol P3 {
    associatedtype A: P3
}

// CHECK-LABEL: StructDecl name=Basic
// CHECK: (normal_conformance type=Basic protocol=P1
// CHECK-NEXT: (assoc_type req=A type=Int)
// CHECK-NEXT: (value req=f() witness=main.(file).Basic.f()@{{.*}}))
struct Basic: P1 {
    typealias A = Int
    func f() -> Int { fatalError() }
}

// Recursive conformances should have finite output.

// CHECK-LABEL: StructDecl name=Recur
// CHECK-NEXT: (normal_conformance type=Recur protocol=P2
// CHECK-NEXT:   (assoc_type req=A type=Recur)
// CHECK-NEXT:   (assoc_type req=B type=Recur)
// CHECK-NEXT:   (normal_conformance type=Recur protocol=P2 (details printed above))
// CHECK-NEXT:   (normal_conformance type=Recur protocol=P2 (details printed above)))
struct Recur: P2 {
    typealias A = Recur
    typealias B = Recur
}

// The full information about a conformance doesn't need to be printed twice.

// CHECK-LABEL: StructDecl name=NonRecur
// CHECK-NEXT: (normal_conformance type=NonRecur protocol=P2
// CHECK-NEXT:   (assoc_type req=A type=Recur)
// CHECK-NEXT:   (assoc_type req=B type=Recur)
// CHECK-NEXT:   (normal_conformance type=Recur protocol=P2
// CHECK-NEXT:     (assoc_type req=A type=Recur)
// CHECK-NEXT:     (assoc_type req=B type=Recur)
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
// CHECK-NEXT:   (assoc_type req=A type=T)
// CHECK-NEXT:   (value req=f() witness=main.(file).Generic.f()@{{.*}})
// CHECK-NEXT:   conforms_to: T P1)
extension Generic: P1 where T: P1 {
    typealias A = T
    func f() -> T { fatalError() }
}


// Satisfying associated types with requirements with generic params
class Super<T, U> {}
// CHECK-LABEL: ExtensionDecl line={{.*}} base=Super<T, U>
// CHECK-NEXT: (normal_conformance type=Super<T, U> protocol=P2
// CHECK-NEXT:   (assoc_type req=A type=T)
// CHECK-NEXT:   (assoc_type req=B type=T)
// CHECK-NEXT:   (abstract_conformance protocol=P2)
// CHECK-NEXT:   (abstract_conformance protocol=P2)
// CHECK-NEXT:   conforms_to: T P2
// CHECK-NEXT:   conforms_to: U P2)
extension Super: P2 where T: P2, U: P2 {
    typealias A = T
    typealias B = T
}

// Inherited/specialized conformances.
// CHECK-LABEL: ClassDecl name=Sub
// CHECK-NEXT: (inherited_conformance type=Sub protocol=P2
// CHECK-NEXT:   (specialized_conformance type=Super<NonRecur, Recur> protocol=P2
// CHECK-NEXT:      (substitution_map generic_signature=<T, U where T : P2, U : P2>
// CHECK-NEXT:         (substitution T -> NonRecur)
// CHECK-NEXT:         (substitution U -> Recur)
// CHECK-NEXT:         (conformance type=T
// CHECK-NEXT:            (normal_conformance type=NonRecur protocol=P2
// CHECK-NEXT:              (assoc_type req=A type=Recur)
// CHECK-NEXT:              (assoc_type req=B type=Recur)
// CHECK-NEXT:              (normal_conformance type=Recur protocol=P2
// CHECK-NEXT:                (assoc_type req=A type=Recur)
// CHECK-NEXT:                (assoc_type req=B type=Recur)
// CHECK-NEXT:                (normal_conformance type=Recur protocol=P2 (details printed above))
// CHECK-NEXT:                (normal_conformance type=Recur protocol=P2 (details printed above)))
// CHECK-NEXT:              (normal_conformance type=Recur protocol=P2 (details printed above))))
// CHECK-NEXT:         (conformance type=U
// CHECK-NEXT:            (normal_conformance type=Recur protocol=P2 (details printed above))))
// CHECK-NEXT:     conforms_to: NonRecur P2
// CHECK-NEXT:     conforms_to: Recur P2
// CHECK-NEXT:     (normal_conformance type=Super<T, U> protocol=P2
// CHECK-NEXT:       (assoc_type req=A type=T)
// CHECK-NEXT:       (assoc_type req=B type=T)
// CHECK-NEXT:       (abstract_conformance protocol=P2)
// CHECK-NEXT:       (abstract_conformance protocol=P2)
// CHECK-NEXT:       conforms_to: T P2
// CHECK-NEXT:       conforms_to: U P2)))
class Sub: Super<NonRecur, Recur> {}

// Specialization of a recursive conformance should be okay: recursion detection
// should work through SubstitutionMaps.

// CHECK-LABEL: StructDecl name=RecurGeneric
// CHECK-NEXT: (normal_conformance type=RecurGeneric<T> protocol=P3
// CHECK-NEXT:   (assoc_type req=A type=RecurGeneric<T>)
// CHECK-NEXT:   (normal_conformance type=RecurGeneric<T> protocol=P3 (details printed above)))
struct RecurGeneric<T: P3>: P3 {
    typealias A = RecurGeneric<T>
}

// CHECK-LABEL: StructDecl name=Specialize
// CHECK-NEXT: (normal_conformance type=Specialize protocol=P3
// CHECK-NEXT:   (assoc_type req=A type=RecurGeneric<Specialize>)
// CHECK-NEXT:   (specialized_conformance type=Specialize.A protocol=P3
// CHECK-NEXT:     (substitution_map generic_signature=<T where T : P3>
// CHECK-NEXT:       (substitution T -> Specialize)
// CHECK-NEXT:       (conformance type=T
// CHECK-NEXT:         (normal_conformance type=Specialize protocol=P3 (details printed above))))
// CHECK-NEXT:     (normal_conformance type=RecurGeneric<T> protocol=P3
// CHECK-NEXT:       (assoc_type req=A type=RecurGeneric<T>)
// CHECK-NEXT:       (normal_conformance type=RecurGeneric<T> protocol=P3 (details printed above)))))
struct Specialize: P3 {
    typealias A = RecurGeneric<Specialize>
}
