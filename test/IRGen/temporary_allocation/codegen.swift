// RUN: %target-swift-frontend -primary-file %s -O -emit-ir | %FileCheck %s
// REQUIRES: optimized_stdlib

@_silgen_name("blackHole")
func blackHole(_ value: UnsafeMutableRawPointer?) -> Void

// MARK: Pointer width
do {
  let ptr = UnsafeMutableRawPointer.allocate(byteCount: 1, alignment: 1)
  blackHole(ptr)
  ptr.deallocate()
}

// MARK: Trivial Cases

// CHECK: [[ONE_BYTE_PTR_RAW:%temp_alloc[0-9]*]] = alloca i8, align 1
// CHECK: [[FIVE_BYTE_PTR_RAW:%temp_alloc[0-9]*]] = alloca [5 x i8], align 1
// CHECK: [[ONE_KB_PTR_RAW:%temp_alloc[0-9]*]] = alloca [1024 x i8], align 8
// CHECK: [[INT_PTR_RAW:%temp_alloc[0-9]*]] = alloca [16 x i8], align 4
// CHECK: [[INT_PTR_RAW2:%temp_alloc[0-9]*]] = alloca [16 x i8], align 4
// CHECK: [[VOID_PTR_RAW:%temp_alloc[0-9]*]] = alloca [2 x i8], align 1
// CHECK: [[ONE_KB_RAND_PTR_RAW:%temp_alloc[0-9]*]] = alloca [1024 x i8], align 16

// CHECK: call swiftcc void @blackHole(ptr {{%.*}})


withUnsafeTemporaryAllocation(byteCount: 0, alignment: 1) { buffer in
  blackHole(buffer.baseAddress)
}
// CHECK: [[ZERO_BYTE_PTR_RAW:%temp_alloc[0-9]*]] = alloca i8, align 1
// CHECK: call swiftcc void @blackHole(ptr nonnull [[ZERO_BYTE_PTR_RAW]])

withUnsafeTemporaryAllocation(byteCount: 1, alignment: 1) { buffer in
  blackHole(buffer.baseAddress)
}
// CHECK: call swiftcc void @blackHole(ptr nonnull [[ONE_BYTE_PTR_RAW]])

withUnsafeTemporaryAllocation(byteCount: 5, alignment: 1) { buffer in
  blackHole(buffer.baseAddress)
}
// CHECK: call swiftcc void @blackHole(ptr nonnull [[FIVE_BYTE_PTR_RAW]])

withUnsafeTemporaryAllocation(byteCount: 1024, alignment: 8) { buffer in
  blackHole(buffer.baseAddress)
}
// CHECK: call swiftcc void @blackHole(ptr nonnull [[ONE_KB_PTR_RAW]])

// MARK: Typed buffers

withUnsafeTemporaryAllocation(of: Int32.self, capacity: 4) { buffer in
  blackHole(buffer.baseAddress)
}
// CHECK: call swiftcc void @blackHole(ptr nonnull [[INT_PTR_RAW]])

_withUnprotectedUnsafeTemporaryAllocation(of: Int32.self, capacity: 4) { buffer in
  blackHole(buffer.baseAddress)
}
// CHECK: call swiftcc void @blackHole(ptr nonnull [[INT_PTR_RAW2]])

withUnsafeTemporaryAllocation(of: Void.self, capacity: 2) { buffer in
  blackHole(buffer.baseAddress)
}
// CHECK: call swiftcc void @blackHole(ptr nonnull [[VOID_PTR_RAW]])

// MARK: Alignment unknown at compile-time

withUnsafeTemporaryAllocation(byteCount: 1024, alignment: Int.random(in: 0 ..< 16)) { buffer in
  blackHole(buffer.baseAddress)
}
// CHECK: call swiftcc void @blackHole(ptr nonnull [[ONE_KB_RAND_PTR_RAW]])
