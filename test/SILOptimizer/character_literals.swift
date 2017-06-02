// RUN: %target-swift-frontend -parse-as-library -O -emit-ir  %s | %FileCheck %s
// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib,CPU=x86_64

// This is an end-to-end test to ensure that the optimizer generates
// a simple literal for character literals.

// CHECK-LABEL: define {{.*}}charArray
// CHECK:  store <2 x i64> <i64 97, i64 98>
// CHECK:  store <2 x i64> <i64 99, i64 100>
// CHECK:  ret
public func charArray(_ i: Int) -> [Character] {
  return [ "a", "b", "c", "d" ]
}

// CHECK-LABEL: define {{.*}}singleChar
// CHECK: ret {{.*}} 97
public func singleChar() -> Character {
  return "a"
}

// FIXME: The following part of this test is temporarily disabled.

// CHECK-LABEL: define {{.*}}singleNonAsciiChar
// NOCHECK: ret {{.*}} -5924890
public func singleNonAsciiChar() -> Character {
  return "日"
}
