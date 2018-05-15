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

// CHECK-LABEL: StructDecl name=Basic
// CHECK: (normal_conformance type=Basic protocol=P1
// CHECK-NEXT: (assoc_type req=A type=Basic.A)
// CHECK-NEXT: (value req=f() witness=main.(file).Basic.f()@{{.*}}))
struct Basic: P1 {
    typealias A = Int
    func f() -> Int { fatalError() }
}
