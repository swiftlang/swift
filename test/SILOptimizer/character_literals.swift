// RUN: %target-swift-frontend -parse-as-library -O -target-cpu core2 -emit-ir  %s | %FileCheck %s
// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib,CPU=x86_64

// This is an end-to-end test to ensure that the optimizer generates
// a simple literal for character literals.

// Please note: this test targets "core2" to ensure consistent output
// on all x86 host processors.

// CHECK-LABEL: define {{.*}}charArray
// CHECK:  store i64 97
// CHECK:  store i64 98
// CHECK:  store i64 99
// CHECK:  store i64 100
// CHECK:  ret
public func charArray(_ i: Int) -> [Character] {
  return [ "a", "b", "c", "d" ]
}

// CHECK-LABEL: define {{.*}}singleChar
// CHECK: tail call swiftcc {{.*}}singleChar
// CHECK-LABEL: define {{.*}}singleNonAsciiChar
// CHECK: tail call swiftcc {{.*}}singleNonAsciiChar

// CHECK-LABEL: define linkonce_odr hidden {{.*}}singleChar
// CHECK: ret {{.*}} 97
public func singleChar() -> Character {
  return "a"
}

// NOTE: 10852326 is base-10 little-endian for E6 97 A5, the encoding of U+65E5
//
// CHECK-LABEL: define linkonce_odr hidden {{.*}}singleNonAsciiChar
// CHECK: ret {{.*}} 10852326
public func singleNonAsciiChar() -> Character {
  return "日"
}
