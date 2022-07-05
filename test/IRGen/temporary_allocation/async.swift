// RUN: %target-swift-frontend -primary-file %s -O -emit-ir -disable-availability-checking | %FileCheck %s
// REQUIRES: concurrency

@_silgen_name("blackHole")
func blackHole(_ value: UnsafeMutableRawPointer?) -> Void

func f() async {
  withUnsafeTemporaryAllocation(byteCount: 123, alignment: 1) { buffer in
    blackHole(buffer.baseAddress)
  }
}
// CHECK: alloca [123 x i8], align 1
// CHECK-NOT: swift_task_alloc
