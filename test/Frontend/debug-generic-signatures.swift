// RUN: %target-swift-frontend -typecheck -debug-generic-signatures %s 2>&1 | %FileCheck %s

// CHECK-LABEL: main.(file).P1@
// CHECK: Requirement signature: <Self>
protocol P1 {
    associatedtype A
    func f() -> A
}

// Recursion, and where clauses.
// CHECK-LABEL: main.(file).P2@
// CHECK-NEXT: Requirement signature: <Self where Self.[P2]A : P2, Self.[P2]B : P2, Self.[P2]A.[P2]A == Self.[P2]B.[P2]A>
protocol P2 {
    associatedtype A: P2
    associatedtype B: P2 where Self.A.A == Self.B.A
}

// Simpler recursion
// CHECK-LABEL: main.(file).P3@
// CHECK-NEXT: Requirement signature: <Self where Self.[P3]A : P3>
protocol P3 {
    associatedtype A: P3
}

// CHECK-LABEL: StructDecl name=Basic
// CHECK: (normal_conformance type="Basic" protocol="P1"
// CHECK-NEXT: (assoc_type req="A" type="Int")
// CHECK-NEXT: (value req="f()" witness="main.(file).Basic.f()@{{.*}}")
struct Basic: P1 {
    typealias A = Int
    func f() -> Int { fatalError() }
}

// Recursive conformances should have finite output.

// CHECK-LABEL: StructDecl name=Recur
// CHECK-NEXT: (builtin_conformance type="Recur" protocol="Copyable"{{.*}})
// CHECK-NEXT: (builtin_conformance type="Recur" protocol="Escapable"{{.*}})
// CHECK-NEXT: (normal_conformance type="Recur" protocol="P2"
// CHECK-NEXT:   (assoc_type req="A" type="Recur")
// CHECK-NEXT:   (assoc_type req="B" type="Recur")
// CHECK-NEXT:   (assoc_conformance type="Self" proto="Copyable"
// CHECK-NEXT:     (builtin_conformance type="Recur" protocol="Copyable"{{.*}}))
// CHECK-NEXT:   (assoc_conformance type="Self" proto="Escapable"
// CHECK-NEXT:     (builtin_conformance type="Recur" protocol="Escapable"{{.*}}))
// CHECK-NEXT:   (assoc_conformance type="Self.A" proto="P2"
// CHECK-NEXT:     (normal_conformance type="Recur" protocol="P2"{{.*}} <details printed above>))
// CHECK-NEXT:   (assoc_conformance type="Self.B" proto="P2"
// CHECK-NEXT:     (normal_conformance type="Recur" protocol="P2"{{.*}} <details printed above>)))
struct Recur: P2 {
    typealias A = Recur
    typealias B = Recur
}

// The full information about a conformance doesn't need to be printed twice.

// CHECK-LABEL: StructDecl name=NonRecur
// CHECK-NEXT: (builtin_conformance type="NonRecur" protocol="Copyable"{{.*}})
// CHECK-NEXT: (builtin_conformance type="NonRecur" protocol="Escapable"{{.*}})
// CHECK-NEXT: (normal_conformance type="NonRecur" protocol="P2"
// CHECK-NEXT:   (assoc_type req="A" type="Recur")
// CHECK-NEXT:   (assoc_type req="B" type="Recur")
// CHECK-NEXT:   (assoc_conformance type="Self" proto="Copyable"
// CHECK-NEXT:     (builtin_conformance type="NonRecur" protocol="Copyable"{{.*}}))
// CHECK-NEXT:   (assoc_conformance type="Self" proto="Escapable"
// CHECK-NEXT:     (builtin_conformance type="NonRecur" protocol="Escapable"{{.*}}))
// CHECK-NEXT:   (assoc_conformance type="Self.A" proto="P2"
// CHECK-NEXT:     (normal_conformance type="Recur" protocol="P2"
// CHECK-NEXT:       (assoc_type req="A" type="Recur")
// CHECK-NEXT:       (assoc_type req="B" type="Recur")
// CHECK-NEXT:       (assoc_conformance type="Self" proto="Copyable"
// CHECK-NEXT:         (builtin_conformance type="Recur" protocol="Copyable"{{.*}}))
// CHECK-NEXT:       (assoc_conformance type="Self" proto="Escapable"
// CHECK-NEXT:         (builtin_conformance type="Recur" protocol="Escapable"{{.*}}))
// CHECK-NEXT:       (assoc_conformance type="Self.A" proto="P2"
// CHECK-NEXT:         (normal_conformance type="Recur" protocol="P2"{{.*}} <details printed above>))
// CHECK-NEXT:       (assoc_conformance type="Self.B" proto="P2"
// CHECK-NEXT:         (normal_conformance type="Recur" protocol="P2"{{.*}} <details printed above>))))
// CHECK-NEXT:   (assoc_conformance type="Self.B" proto="P2"
// CHECK-NEXT:     (normal_conformance type="Recur" protocol="P2"{{.*}} <details printed above>)))
struct NonRecur: P2 {
    typealias A = Recur
    typealias B = Recur
}

// Conditional conformance.

// CHECK: Generic signature: <T>
// CHECK-NEXT: Canonical generic signature: <τ_0_0>
// CHECK-LABEL: ExtensionDecl line={{.*}} base=Generic
struct Generic<T> {}
// CHECK-LABEL: ExtensionDecl line={{.*}} base=Generic
// CHECK-NEXT: (normal_conformance type="Generic<T>" protocol="P1"
// CHECK-NEXT:   (assoc_type req="A" type="T")
// CHECK-NEXT:   (value req="f()" witness="main.(file).Generic extension.f()@{{.*}}")
// CHECK-NEXT:   (assoc_conformance type="Self" proto="Copyable"
// CHECK-NEXT:     (builtin_conformance type="Generic<T>" protocol="Copyable"{{.*}}))
// CHECK-NEXT:   (assoc_conformance type="Self" proto="Escapable"
// CHECK-NEXT:     (builtin_conformance type="Generic<T>" protocol="Escapable"{{.*}}))
// CHECK-NEXT:   (assoc_conformance type="Self.A" proto="Copyable"
// CHECK-NEXT:     (abstract_conformance type="T" protocol="Copyable"))
// CHECK-NEXT:   (assoc_conformance type="Self.A" proto="Escapable"
// CHECK-NEXT:     (abstract_conformance type="T" protocol="Escapable"))
// CHECK-NEXT:   (requirement "T" conforms_to "P1"))
extension Generic: P1 where T: P1 {
    typealias A = T
    func f() -> T { fatalError() }
}


// Satisfying associated types with requirements with generic params

// CHECK: Generic signature: <T, U>
// CHECK-NEXT: Canonical generic signature: <τ_0_0, τ_0_1>
// CHECK-LABEL: ExtensionDecl line={{.*}} base=Super
class Super<T, U> {}
// CHECK-LABEL: ExtensionDecl line={{.*}} base=Super
// CHECK-NEXT: (normal_conformance type="Super<T, U>" protocol="P2"
// CHECK-NEXT:   (assoc_type req="A" type="T")
// CHECK-NEXT:   (assoc_type req="B" type="T")
// CHECK-NEXT:   (assoc_conformance type="Self" proto="Copyable"
// CHECK-NEXT:     (builtin_conformance type="Super<T, U>" protocol="Copyable"{{.*}}))
// CHECK-NEXT:   (assoc_conformance type="Self" proto="Escapable"
// CHECK-NEXT:     (builtin_conformance type="Super<T, U>" protocol="Escapable"{{.*}}))
// CHECK-NEXT:   (assoc_conformance type="Self.A" proto="P2"
// CHECK-NEXT:     (abstract_conformance type="T" protocol="P2"))
// CHECK-NEXT:   (assoc_conformance type="Self.B" proto="P2"
// CHECK-NEXT:     (abstract_conformance type="T" protocol="P2"))
// CHECK-NEXT:   (requirement "T" conforms_to "P2")
// CHECK-NEXT:   (requirement "U" conforms_to "P2"))
extension Super: P2 where T: P2, U: P2 {
    typealias A = T
    typealias B = T
}

// Inherited/specialized conformances.
// CHECK-LABEL: ClassDecl name=Sub
// CHECK-NEXT: (builtin_conformance type="Sub" protocol="Copyable"{{.*}})
// CHECK-NEXT: (builtin_conformance type="Sub" protocol="Escapable"{{.*}})
// CHECK-NEXT: (inherited_conformance type="Sub" protocol="P2"
// CHECK-NEXT:   (specialized_conformance type="Super<NonRecur, Recur>" protocol="P2"
// CHECK-NEXT:     (substitution_map generic_signature=<T, U where T : P2, U : P2>
// CHECK-NEXT:       (substitution T ->
// CHECK-NEXT:         (struct_type decl="main.(file).NonRecur@{{.*}}"))
// CHECK-NEXT:       (substitution U ->
// CHECK-NEXT:         (struct_type decl="main.(file).Recur@{{.*}}"))
// CHECK-NEXT:       (conformance type="T"
// CHECK-NEXT:         (normal_conformance type="NonRecur" protocol="P2"
// CHECK-NEXT:           (assoc_type req="A" type="Recur")
// CHECK-NEXT:           (assoc_type req="B" type="Recur")
// CHECK-NEXT:           (assoc_conformance type="Self" proto="Copyable"
// CHECK-NEXT:             (builtin_conformance type="NonRecur" protocol="Copyable"{{.*}}))
// CHECK-NEXT:           (assoc_conformance type="Self" proto="Escapable"
// CHECK-NEXT:             (builtin_conformance type="NonRecur" protocol="Escapable"{{.*}}))
// CHECK-NEXT:           (assoc_conformance type="Self.A" proto="P2"
// CHECK-NEXT:             (normal_conformance type="Recur" protocol="P2"
// CHECK-NEXT:               (assoc_type req="A" type="Recur")
// CHECK-NEXT:               (assoc_type req="B" type="Recur")
// CHECK-NEXT:               (assoc_conformance type="Self" proto="Copyable"
// CHECK-NEXT:                 (builtin_conformance type="Recur" protocol="Copyable"{{.*}}))
// CHECK-NEXT:               (assoc_conformance type="Self" proto="Escapable"
// CHECK-NEXT:                 (builtin_conformance type="Recur" protocol="Escapable"{{.*}}))
// CHECK-NEXT:               (assoc_conformance type="Self.A" proto="P2"
// CHECK-NEXT:                 (normal_conformance type="Recur" protocol="P2"{{.*}} <details printed above>))
// CHECK-NEXT:               (assoc_conformance type="Self.B" proto="P2"
// CHECK-NEXT:                 (normal_conformance type="Recur" protocol="P2"{{.*}} <details printed above>))))
// CHECK-NEXT:           (assoc_conformance type="Self.B" proto="P2"
// CHECK-NEXT:             (normal_conformance type="Recur" protocol="P2"{{.*}} <details printed above>))))
// CHECK-NEXT:       (conformance type="U"
// CHECK-NEXT:         (normal_conformance type="Recur" protocol="P2"{{.*}} <details printed above>)))
// CHECK-NEXT:     (<conditional requirements unable to be computed>)
// CHECK-NEXT:     (normal_conformance type="Super<T, U>" protocol="P2"
// CHECK-NEXT:       (assoc_type req="A" type="T")
// CHECK-NEXT:       (assoc_type req="B" type="T")
// CHECK-NEXT:       (assoc_conformance type="Self" proto="Copyable"
// CHECK-NEXT:         (builtin_conformance type="Super<T, U>" protocol="Copyable"{{.*}}))
// CHECK-NEXT:       (assoc_conformance type="Self" proto="Escapable"
// CHECK-NEXT:         (builtin_conformance type="Super<T, U>" protocol="Escapable"{{.*}}))
// CHECK-NEXT:       (assoc_conformance type="Self.A" proto="P2"
// CHECK-NEXT:         (abstract_conformance type="T" protocol="P2"))
// CHECK-NEXT:       (assoc_conformance type="Self.B" proto="P2"
// CHECK-NEXT:         (abstract_conformance type="T" protocol="P2"))
// CHECK-NEXT:       (requirement "T" conforms_to "P2")
// CHECK-NEXT:       (requirement "U" conforms_to "P2"))))
class Sub: Super<NonRecur, Recur> {}

// Specialization of a recursive conformance should be okay: recursion detection
// should work through SubstitutionMaps.

// CHECK-LABEL: StructDecl name=RecurGeneric
// CHECK-NEXT: (builtin_conformance type="RecurGeneric<T>" protocol="Copyable"{{.*}})
// CHECK-NEXT: (builtin_conformance type="RecurGeneric<T>" protocol="Escapable"{{.*}})
// CHECK-NEXT: (normal_conformance type="RecurGeneric<T>" protocol="P3"
// CHECK-NEXT:   (assoc_type req="A" type="RecurGeneric<T>")
// CHECK-NEXT:   (assoc_conformance type="Self" proto="Copyable"
// CHECK-NEXT:     (builtin_conformance type="RecurGeneric<T>" protocol="Copyable"{{.*}}))
// CHECK-NEXT:   (assoc_conformance type="Self" proto="Escapable"
// CHECK-NEXT:     (builtin_conformance type="RecurGeneric<T>" protocol="Escapable"{{.*}}))
// CHECK-NEXT:   (assoc_conformance type="Self.A" proto="P3"
// CHECK-NEXT:     (normal_conformance type="RecurGeneric<T>" protocol="P3"{{.*}} <details printed above>)))
struct RecurGeneric<T: P3>: P3 {
    typealias A = RecurGeneric<T>
}

// CHECK-LABEL: StructDecl name=Specialize
// CHECK-NEXT: (builtin_conformance type="Specialize" protocol="Copyable"{{.*}})
// CHECK-NEXT: (builtin_conformance type="Specialize" protocol="Escapable"{{.*}})
// CHECK-NEXT: (normal_conformance type="Specialize" protocol="P3"
// CHECK-NEXT:   (assoc_type req="A" type="RecurGeneric<Specialize>")
// CHECK-NEXT:   (assoc_conformance type="Self" proto="Copyable"
// CHECK-NEXT:     (builtin_conformance type="Specialize" protocol="Copyable"{{.*}}))
// CHECK-NEXT:   (assoc_conformance type="Self" proto="Escapable"
// CHECK-NEXT:     (builtin_conformance type="Specialize" protocol="Escapable"{{.*}}))
// CHECK-NEXT:   (assoc_conformance type="Self.A" proto="P3"
// CHECK-NEXT:     (specialized_conformance type="Specialize.A" protocol="P3"
// CHECK-NEXT:       (substitution_map generic_signature=<T where T : P3>
// CHECK-NEXT:         (substitution T ->
// CHECK-NEXT:           (struct_type decl="main.(file).Specialize@{{.*}}"))
// CHECK-NEXT:         (conformance type="T"
// CHECK-NEXT:           (normal_conformance type="Specialize" protocol="P3"{{.*}} <details printed above>)))
// CHECK-NEXT:       (normal_conformance type="RecurGeneric<T>" protocol="P3"
// CHECK-NEXT:         (assoc_type req="A" type="RecurGeneric<T>")
// CHECK-NEXT:         (assoc_conformance type="Self" proto="Copyable"
// CHECK-NEXT:           (builtin_conformance type="RecurGeneric<T>" protocol="Copyable"{{.*}}))
// CHECK-NEXT:         (assoc_conformance type="Self" proto="Escapable"
// CHECK-NEXT:           (builtin_conformance type="RecurGeneric<T>" protocol="Escapable"{{.*}}))
// CHECK-NEXT:         (assoc_conformance type="Self.A" proto="P3"
// CHECK-NEXT:           (normal_conformance type="RecurGeneric<T>" protocol="P3"{{.*}} <details printed above>))))))
struct Specialize: P3 {
    typealias A = RecurGeneric<Specialize>
}
