// RUN: %target-swift-frontend -emit-ir -target x86_64-apple-macos99.99 %s | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-sans-workaround %s
// RUN: %target-swift-frontend -emit-ir -target x86_64-apple-macos12.3 %s | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-with-workaround %s

// REQUIRES: OS=macosx
 
// rdar://90506708: Prior to Swift 5.7, the Swift concurrency runtime had a bug
// that led to memory corruption in cases when an `async let` child task
// would try to use the last 16 bytes of the preallocated slab from the parent
// to seed its own task allocator. When targeting older Apple OSes that shipped
// with this bug in their runtime, we work around the bug by inflating the
// initial context sizes of any async functions used as `async let` entry points
// to ensure that the preallocated space is never used for the initial context.

// CHECK: [[ASYNC_LET_ENTRY:@"\$sSiIeghHd_Sis5Error_pIegHrzo_TRTATu"]]
// CHECK-with-workaround-SAME: = internal {{.*}} %swift.async_func_pointer <{ {{.*}}, i32 6{{[0-9][0-9]}} }>
// CHECK-sans-workaround-SAME: = internal {{.*}} %swift.async_func_pointer <{ {{.*}}, i32 {{[0-9][0-9]}} }>

// CHECK: swift_asyncLet_begin{{.*}}[[ASYNC_LET_ENTRY]]
public func foo(x: Int, y: Int) async -> Int {
    async let z = x + y
    return await z
}
