// RUN: %target-swift-frontend -parse-as-library -O -module-name=test %s -emit-sil | %FileCheck %s

// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib

// Check that the array storage is not released before the elements are copied
// into it, if the resulting array is not used.
// rdar://191252169

// CHECK-LABEL: sil @$s4test0A9DiscardedyySv_S2itF :
// CHECK-NOT:     {{strong_release|release_value}}
// CHECK:       bb{{[0-9]+}}([[BUF:%[0-9]+]] : $_ContiguousArrayBuffer<UInt8>):
// CHECK-NOT:     {{strong_release|release_value}}
// CHECK:         fix_lifetime [[BUF]]
// CHECK:         release_value [[BUF]]
// CHECK-NOT:     {{strong_release|release_value}}
// CHECK:       } // end sil function '$s4test0A9DiscardedyySv_S2itF'
public func testDiscarded(_ p: UnsafeMutableRawPointer, _ count: Int, _ iterations: Int) {
  for _ in 0..<iterations {
    _ = Array(UnsafeBufferPointer(start: p.assumingMemoryBound(to: UInt8.self), count: count))
  }
}
