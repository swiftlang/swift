// RUN: %target-swift-frontend -typecheck -debug-generic-signatures %s 2>&1 | %FileCheck %s

protocol P {
  associatedtype T
}

protocol Base {
  associatedtype A
  associatedtype B
  associatedtype C
  associatedtype D : P where B == D.T
}

// CHECK-LABEL: connected_components_concrete.(file).Derived1@
// CHECK-LABEL: Requirement signature: <Self where Self : Base, Self.[Base]A == Int, Self.[Base]B == Int, Self.[Base]C == Int>
protocol Derived1 : Base where A == B, A == C, A == Int {}

// CHECK-LABEL: connected_components_concrete.(file).Derived2@
// CHECK-LABEL: Requirement signature: <Self where Self : Base, Self.[Base]A == Int, Self.[Base]B == Int, Self.[Base]C == Int>
protocol Derived2 : Base where A == D.T, A == C, B == Int {}

// CHECK-LABEL: connected_components_concrete.(file).Derived3@
// CHECK-LABEL: Requirement signature: <Self where Self : Base, Self.[Base]A == Int, Self.[Base]B == Int, Self.[Base]C == Int>
protocol Derived3 : Base where A == B, B == C, A == Int {}

// CHECK-LABEL: connected_components_concrete.(file).Derived4@
// CHECK-LABEL: Requirement signature: <Self where Self : Base, Self.[Base]A == Int, Self.[Base]B == Int, Self.[Base]C == Int>
protocol Derived4 : Base where A == Int, B == Int, C == Int {}

// CHECK-LABEL: connected_components_concrete.(file).Derived5@
// CHECK-LABEL: Requirement signature: <Self where Self : Base, Self.[Base]A == Int, Self.[Base]B == Int, Self.[Base]C == Int>
protocol Derived5 : Base where A == Int, D.T == Int, C == Int {}
