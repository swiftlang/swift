// RUN: %target-swift-frontend %use_no_opaque_pointers %s -emit-ir -disable-availability-checking -disable-objc-interop | %FileCheck %s
// RUN: %target-swift-frontend %s -emit-ir -disable-availability-checking -disable-objc-interop

// REQUIRES: concurrency

public dynamic func number() async -> Int {
    return 100
}

@_dynamicReplacement(for: number())
internal func _replacement_number() async -> Int {
    return 200
}

// rdar://78284346 - Dynamic replacement should use musttail
//                   for tail calls from swifttailcc to swifttailcc
// CHECK-LABEL: define {{.*}} swifttailcc void @"$s25async_dynamic_replacement01_C7_numberSiyYaFTI"
// CHECK: musttail call swifttailcc void
// CHECK-NEXT: ret void

public func calls_number() async -> Int {
  await number()
}

// CHECK-LABEL: define {{.*}}swifttailcc void @"$s25async_dynamic_replacement32indirectReturnDynamicReplaceableSi_S6ityYaKF"(<{ %TSi, %TSi, %TSi, %TSi, %TSi, %TSi, %TSi }>* {{.*}}%0, %swift.context* swiftasync %1)
// CHECK: forward_to_replaced:
// CHECK: musttail call swifttailcc void {{.*}}(<{ %TSi, %TSi, %TSi, %TSi, %TSi, %TSi, %TSi }>* noalias nocapture %0, %swift.context* swiftasync {{.*}})
public dynamic func indirectReturnDynamicReplaceable() async throws -> (Int, Int, Int, Int, Int, Int, Int) {
    return (0, 0, 0, 0, 0, 0, 0)
}
