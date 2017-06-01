// RUN: %target-swift-frontend -parse-as-library -O -emit-ir  %s | %FileCheck %s
// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib,CPU=x86_64

// This is an end-to-end test to ensure that the optimizer generates
// a simple literal for character literals.

// CHECK-LABEL: define {{.*}}charArray
// CHECK:  store <2 x i64> <i64 -159, i64 -158>
// CHECK:  store <2 x i64> <i64 -157, i64 -156>
// CHECK:  ret
public func charArray(_ i: Int) -> [Character] {
  return [ "a", "b", "c", "d" ]
}

// CHECK-LABEL: define {{.*}}singleChar
// CHECK: ret {{.*}} -159
public func singleChar() -> Character {
  return "a"
}

// CHECK-LABEL: define {{.*}}singleNonAsciiChar
// CHECK: ret {{.*}} -5924890
public func singleNonAsciiChar() -> Character {
  return "日"
}
