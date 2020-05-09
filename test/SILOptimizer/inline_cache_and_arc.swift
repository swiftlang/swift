// RUN: %target-swift-frontend -parse-as-library -enable-spec-devirt -O -emit-sil  %s | %FileCheck %s
// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib,CPU=x86_64

// Test inline cache with a global class. Make sure the retain|release pair
// for the fast path is removed in the loop.

open class A
{
  open func f(_ a: Int) -> Int
  {
    return a + 1
  }
}

var x = 0
public var a = A()

public func testit() {
// CHECK-LABEL: sil @{{.*}}testityyF
// CHECK:     bb0:
// CHECK-NOT:   {{.*(retain|release).*}}
// CHECK:     bb1{{.*}}:
// CHECK-NOT:   {{.*(retain|release).*}}
// CHECK:       return
// CHECK:     bb2{{.*}}:
// CHECK-NOT:   {{.*(retain|release|apply).*}}
// CHECK:       br bb1
// CHECK:     bb3{{.*}}:
// CHECK-NEXT:  class_method
// CHECK-NEXT:  strong_retain
// CHECK-NEXT:  apply
// CHECK-NEXT:  strong_release
// CHECK-NEXT:  br bb1

  x = a.f(x)
}

