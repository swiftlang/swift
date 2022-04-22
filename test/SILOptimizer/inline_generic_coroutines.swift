// RUN: %target-swift-frontend -parse-as-library -O -emit-sil  %s | %FileCheck %s
// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib

@inline(never)
func useit<T>(_ t: T) {
  print(t)
}

// Check that we inline the Array.subscript.read coroutine

// CHECK-LABEL: sil @{{.*}}testit
// CHECK-NOT:     begin_apply
// CHECK-NOT:     end_apply
// CHECK:       } // end sil function {{.*}}testit
public func testit<T>(_ a: [T]) {
  for t in a {
    useit(t)
  }
}

// Check that we inline the ManagedBufferPointer.header.read coroutine

public final class MyBuffer<Element> {
    typealias Manager = ManagedBufferPointer<Int, Element>

    // CHECK-LABEL: sil @{{.*}}MyBuffer{{.*}}countSivg
    // CHECK-NOT:     begin_apply
    // CHECK-NOT:     end_apply
    // CHECK:       } // end sil function {{.*}}MyBuffer{{.*}}countSivg
    public var count: Int {
        return Manager(unsafeBufferObject: self).header
    }
}

