// RUN: %target-typecheck-verify-swift -debug-generic-signatures 2>&1 | %FileCheck %s

protocol Base<T> {
  associatedtype T
}

// CHECK-LABEL: .Derived@
// CHECK-NEXT: Requirement signature: <Self where Self : Base>
protocol Derived: Base & DoesNotExist { // expected-error {{cannot find type 'DoesNotExist' in scope}}
  func position(_: T)
}

// CHECK-LABEL: .OtherDerived@
// CHECK-NEXT: Requirement signature: <Self where Self : Base>
protocol OtherDerived: Base<DoesNotExist> {} // expected-error {{cannot find type 'DoesNotExist' in scope}}