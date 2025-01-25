// RUN: %target-run-simple-swift(-enable-experimental-feature RawLayout)
// REQUIRES: executable_test
// REQUIRES: swift_feature_RawLayout
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: CPU=arm64e

import Synchronization

struct NC<T>: ~Copyable { var x: T }

@_rawLayout(like: T, movesAsLike)
struct Test<T>: ~Copyable {}

func isBitwiseBorrowable(_ type: any (~Copyable.Type)) -> Bool {
    let metadataPtr = unsafeBitCast(type,
                                    to: UnsafePointer<UnsafePointer<UInt>>.self)
    let flags = metadataPtr[-1][10]
    return flags & 0x0110_0000 == 0
}

func test(_ type: any (~Copyable.Type)) {
    print("\(isBitwiseBorrowable(type))")
}

protocol P: ~Copyable {}

// CHECK: begin
print("begin")

// CHECK-NEXT: true
test(Int.self)
// CHECK-NEXT: true
test(Any.self)
// CHECK-NEXT: true
test(P.self)
// CHECK-NEXT: true
test(NC<Int>.self)
// CHECK-NEXT: true
test(NC<Any>.self)
// CHECK-NEXT: false
test(Test<Int>.self)
// CHECK-NEXT: false
test(Test<Any>.self)
