// RUN: %target-swift-frontend %s -emit-ir -disable-availability-checking -disable-objc-interop | %FileCheck %s

// Windows does not do swiftailcc
// XFAIL: OS=windows-msvc

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
