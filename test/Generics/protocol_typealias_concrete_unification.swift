// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

protocol P1 {
  typealias T = Array<U>
  associatedtype U
}

protocol P2 {
  typealias T = Array<Int>
}

// Note that the GenericSignatureBuilder did not record the 'T.[P1]U == Int'
// requirement. But I believe this is too aggressive.

// CHECK-LABEL: .foo(_:u:)@
// CHECK-NEXT: Generic signature: <T where T : P1, T : P2, T.[P1]U == Int>
func foo<T : P1 & P2>(_: T, u: T.U) -> Int { return u }

// CHECK-LABEL: .P3@
// CHECK-NEXT: Requirement signature: <Self where Self : P1, Self : P2, Self.[P1]U == Int>
protocol P3 : P1, P2 {}

// This conformance should succeed, and associated type inference should infer
// 'S.U == Int'.
struct S : P3 {}