// RUN: %target-swift-frontend -primary-file %s -O -emit-ir | %FileCheck %s --check-prefix=CHECK-%target-vendor

@_silgen_name("blackHole")
func blackHole(_ value: UnsafeMutableRawPointer?) -> Void

// MARK: Pointer width
do {
  let ptr = UnsafeMutableRawPointer.allocate(byteCount: 1, alignment: 1)
  blackHole(ptr)
  ptr.deallocate()
}
// CHECK: ptrtoint i8* {{.*}} to [[WORD:i[0-9]+]]

// MARK: Trivial Cases

withUnsafeTemporaryAllocation(byteCount: 0, alignment: 1) { buffer in
  blackHole(buffer.baseAddress)
}
// CHECK: [[ZERO_BYTE_PTR_RAW:%temp_alloc[0-9]*]] = alloca i8, align 1
// CHECK: [[ZERO_BYTE_PTR:%[0-9]+]] = ptrtoint i8* [[ZERO_BYTE_PTR_RAW]] to [[WORD]]
// CHECK: call swiftcc void @blackHole([[WORD]] [[ZERO_BYTE_PTR]])

withUnsafeTemporaryAllocation(byteCount: 1, alignment: 1) { buffer in
  blackHole(buffer.baseAddress)
}
// CHECK: [[ONE_BYTE_PTR_RAW:%temp_alloc[0-9]*]] = alloca i8, align 1
// CHECK: [[ONE_BYTE_PTR:%[0-9]+]] = ptrtoint i8* [[ONE_BYTE_PTR_RAW]] to [[WORD]]
// CHECK: call swiftcc void @blackHole([[WORD]] [[ONE_BYTE_PTR]])

withUnsafeTemporaryAllocation(byteCount: 5, alignment: 1) { buffer in
  blackHole(buffer.baseAddress)
}
// CHECK: [[FIVE_BYTE_PTR_RAW:%temp_alloc[0-9]*]] = alloca [5 x i8], align 1
// CHECK: [[FIVE_BYTE_PTR:%[0-9]+]] = ptrtoint [5 x i8]* [[FIVE_BYTE_PTR_RAW]] to [[WORD]]
// CHECK: call swiftcc void @blackHole([[WORD]] [[FIVE_BYTE_PTR]])

withUnsafeTemporaryAllocation(byteCount: 1024, alignment: 8) { buffer in
  blackHole(buffer.baseAddress)
}
// CHECK: [[ONE_KB_PTR_RAW:%temp_alloc[0-9]*]] = alloca [1024 x i8], align 8
// CHECK: [[ONE_KB_PTR:%[0-9]+]] = ptrtoint [1024 x i8]* [[ONE_KB_PTR_RAW]] to [[WORD]]
// CHECK: call swiftcc void @blackHole([[WORD]] [[ONE_KB_PTR]])

// MARK: Alignment unknown at compile-time

withUnsafeTemporaryAllocation(byteCount: 1024, alignment: Int.random(in: 0 ..< 16)) { buffer in
  blackHole(buffer.baseAddress)
}
// CHECK: [[ONE_KB_RAND_PTR_RAW:%temp_alloc[0-9]*]] = alloca [1024 x i8], align 16
// CHECK: [[ONE_KB_RAND_PTR:%[0-9]+]] = ptrtoint [1024 x i8]* [[ONE_KB_RAND_PTR_RAW]] to [[WORD]]
// CHECK: call swiftcc void @blackHole([[WORD]] [[ONE_KB_RAND_PTR]])

// MARK: Typed buffers

withUnsafeTemporaryAllocation(of: Int32.self, capacity: 4) { buffer in
  blackHole(buffer.baseAddress)
}
// CHECK: [[INT_PTR_RAW:%temp_alloc[0-9]*]] = alloca [16 x i8], align 4
// CHECK: [[INT_PTR:%[0-9]+]] = ptrtoint [16 x i8]* [[INT_PTR_RAW]] to [[WORD]]
// CHECK: call swiftcc void @blackHole([[WORD]] [[INT_PTR]])

withUnsafeTemporaryAllocation(of: Void.self, capacity: 2) { buffer in
  blackHole(buffer.baseAddress)
}
// CHECK: [[VOID_PTR_RAW:%temp_alloc[0-9]*]] = alloca [2 x i8], align 1
// CHECK: [[VOID_PTR:%[0-9]+]] = ptrtoint [2 x i8]* [[VOID_PTR_RAW]] to [[WORD]]
// CHECK: call swiftcc void @blackHole([[WORD]] [[VOID_PTR]])

// MARK: Very large allocation

// A large allocation size should produce an OS version check, call to
// swift_stdlib_isStackAllocationSafe(), and then a branch based on the result
// to either stack-allocate or heap-allocate.
withUnsafeTemporaryAllocation(byteCount: 0x0FFF_FFFF, alignment: 1) { buffer in
  blackHole(buffer.baseAddress)
}
// CHECK-apple: [[IS_OS_OK:%[0-9]+]] = call swiftcc i1 @"$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF"
// CHECK-apple: br i1 [[IS_OS_OK]], label %[[OS_OK_BR:[0-9]+]], label %[[UNSAFE_BR:[0-9]+]]

// CHECK-apple: [[UNSAFE_BR]]:
// CHECK-unknown: [[UNSAFE_BR:[0-9]+]]:
// CHECK: [[HEAP_PTR_RAW:%[0-9]+]] = call noalias i8* @swift_slowAlloc([[WORD]] 268435455, [[WORD]] -1)
// CHECK: [[HEAP_PTR:%[0-9]+]] = ptrtoint i8* [[HEAP_PTR_RAW]] to [[WORD]]
// CHECK: call swiftcc void @blackHole([[WORD]] [[HEAP_PTR]])
// CHECK: call void @swift_slowDealloc(i8* [[HEAP_PTR_RAW]], [[WORD]] -1, [[WORD]] -1)

// CHECK-apple: [[OS_OK_BR]]:
// CHECK: [[IS_SAFE:%[0-9]+]] = call zeroext i1 @swift_stdlib_isStackAllocationSafe([[WORD]] 268435455, [[WORD]] 1)
// CHECK: br i1 [[IS_SAFE]], label %[[SAFE_BR:[0-9]+]], label %[[UNSAFE_BR]]

// CHECK: [[SAFE_BR]]:
// CHECK: [[SPSAVE:%spsave[0-9]*]] = call i8* @llvm.stacksave()
// CHECK: [[STACK_PTR_RAW:%temp_alloc[0-9]*]] = alloca [268435455 x i8], align 1
// CHECK: [[STACK_PTR:%[0-9]+]] = ptrtoint [268435455 x i8]* [[STACK_PTR_RAW]] to [[WORD]]
// CHECK: call swiftcc void @blackHole([[WORD]] [[STACK_PTR]])
// CHECK: call void @llvm.stackrestore(i8* [[SPSAVE]])

