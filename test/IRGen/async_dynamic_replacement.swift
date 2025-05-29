// RUN: %target-swift-frontend %s -emit-ir -target %target-swift-5.1-abi-triple -disable-objc-interop | %FileCheck %s

// REQUIRES: concurrency
// LLVM does not support swifttailcc for WebAssembly target for now
// See https://github.com/apple/swift/issues/69333
// UNSUPPORTED: CPU=wasm32

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

// CHECK-LABEL: define {{.*}}swifttailcc void @"$s25async_dynamic_replacement32indirectReturnDynamicReplaceableSi_S6ityYaKF"(ptr {{.*}}%0, ptr swiftasync %1)
// CHECK: forward_to_replaced:
// CHECK: musttail call swifttailcc void {{.*}}(ptr noalias {{(nocapture|captures\(none\))}} %0, ptr swiftasync {{.*}})
public dynamic func indirectReturnDynamicReplaceable() async throws -> (Int, Int, Int, Int, Int, Int, Int) {
    return (0, 0, 0, 0, 0, 0, 0)
}
