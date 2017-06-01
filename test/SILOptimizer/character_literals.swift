// RUN: %target-swift-frontend -parse-as-library -O -emit-ir  %s | %FileCheck %s
// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib,CPU=x86_64

// see https://bugs.swift.org/browse/SR-5064, rdar://32511377
// XFAIL: *

// This is an end-to-end test to ensure that the optimizer generates
// a simple literal for character literals.

// CHECK-LABEL: define {{.*}}charArray
// CHECK-NOT: {{^}}[^ ]
// CHECK:       store i64 9223372036854775649, i64*
// CHECK-NOT: {{^}}[^ ]
// CHECK:       store i64 9223372036854775650, i64*
// CHECK-NOT: {{^}}[^ ]
// CHECK:       store i64 9223372036854775651, i64*
// CHECK-NOT: {{^}}[^ ]
// CHECK:       store i64 9223372036854775652, i64*
// CHECK-NOT: {{^}}[^ ]
// CHECK:       ret
public func charArray(_ i: Int) -> [Character] {
  return [ "a", "b", "c", "d" ]
}

// CHECK-LABEL: define {{.*}}singleChar
// CHECK: ret {{.*}} 9223372036854775649
public func singleChar() -> Character {
  return "a"
}

// CHECK-LABEL: define {{.*}}singleNonAsciiChar
// CHECK: ret {{.*}} 9223372036848850918
public func singleNonAsciiChar() -> Character {
  return "日"
}
