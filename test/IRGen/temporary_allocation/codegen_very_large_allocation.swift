// RUN: %target-swift-frontend -target %target-swift-5.5-abi-triple -primary-file %s -O -emit-ir > %t.ir
// RUN: %FileCheck %s -DWORD=i%target-ptrsize < %t.ir
// RUN: %FileCheck %s --check-prefix=CHECK-NEGATIVE -DWORD=i%target-ptrsize < %t.ir


@_silgen_name("blackHole")
func blackHole(_ value: UnsafeMutableRawPointer?) -> Void

// MARK: Very large allocation

withUnsafeTemporaryAllocation(byteCount: 0x0FFF_FFFF, alignment: 1) { buffer in
  blackHole(buffer.baseAddress)
}
// CHECK: [[HEAP_PTR_RAW:%[0-9]+]] = {{(tail )?}}call noalias ptr @swift_slowAlloc([[WORD]] 268435455, [[WORD]] -1)
// CHECK-NEXT: [[HEAP_PTR:%[0-9]+]] = ptrtoint ptr [[HEAP_PTR_RAW]] to [[WORD]]
// CHECK-NEXT: {{(tail )?}}call swiftcc void @blackHole([[WORD]] [[HEAP_PTR]])
// CHECK-NEXT: {{(tail )?}}call void @swift_slowDealloc(ptr [[HEAP_PTR_RAW]], [[WORD]] -1, [[WORD]] -1)

// CHECK-NEGATIVE-NOT: swift_stdlib_isStackAllocationSafe
// CHECK-NEGATIVE-NOT: ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF
