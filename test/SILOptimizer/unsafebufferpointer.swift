// RUN: %target-swift-frontend -parse-as-library -Osize -emit-ir  %s | %FileCheck %s
// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib

// This is an end-to-end test to ensure that the optimizer generates
// optimal code for UnsafeBufferPointer.

// CHECK-LABEL: define {{.*}}testIteration

// Check if the code contains no traps at all.
// CHECK-NOT: unreachable
public func testIteration(_ p: UnsafeBufferPointer<Int>) -> Int {
  var s = 0

// Check for an optimal loop kernel
// CHECK:       phi
// CHECK-NEXT:  phi
// CHECK-NEXT:  bitcast
// CHECK-NEXT:  load
// CHECK-NEXT:  getelementptr
// CHECK-NEXT:  add
// CHECK-NEXT:  icmp
// CHECK-NEXT:  br
  for x in p {
    s = s &+ x
  }
// CHECK-NOT: unreachable
// CHECK:       phi
// CHECK-NEXT:  ret
// CHECK-NOT: unreachable
  return s
}

// CHECK-LABEL: define {{.*}}testIsEmpty
// CHECK:      entry:
// CHECK-NEXT:   icmp
// CHECK-NEXT:   ret
public func testIsEmpty(_ x: UnsafeBufferPointer<UInt>) -> Bool {
  return x.isEmpty
}

// CHECK-LABEL: define {{.*}}testCount
// CHECK:      entry:
// CHECK-NEXT:   ret
public func testCount(_ x: UnsafeBufferPointer<UInt>) -> Int {
  return x.count
}

