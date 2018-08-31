// RUN: %target-swift-frontend -parse-as-library -O -target-cpu core2 -emit-ir  %s | %FileCheck %s
// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib,CPU=x86_64

// This is an end-to-end test to ensure that the optimizer generates
// a simple literal for character literals.

// Please note: this test targets "core2" to ensure consistent output
// on all x86 host processors.

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

// CHECK-LABEL: define {{.*}}singleNonAsciiChar
// CHECK: ret {{.*}} 26085
public func singleNonAsciiChar() -> Character {
  return "日"
}
