// RUN: %target-swift-frontend -emit-ir -target %target-swift-5.7-abi-triple %s | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-sans-workaround %s
// RUN: %target-swift-frontend -emit-ir -target %target-swift-5.6-abi-triple %s | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-with-workaround %s

// REQUIRES: OS=macosx
// UNSUPPORTED: CPU=arm64e
 
// rdar://90506708: Prior to Swift 5.7, the Swift concurrency runtime had a bug
// that led to memory corruption in cases when an `async let` child task
// would try to use the last 16 bytes of the preallocated slab from the parent
// to seed its own task allocator. When targeting older Apple OSes that shipped
// with this bug in their runtime, we work around the bug by inflating the
// initial context sizes of any async functions used as `async let` entry points
// to ensure that the preallocated space is never used for the initial context.

// CHECK: [[ASYNC_LET_ENTRY_FOO:@"\$s32async_let_back_deploy_workaround3foo1x1yS2i_SitYaFSiyYaYbcfu_TATu"]]
// CHECK-with-workaround-SAME: = internal {{.*}} %swift.async_func_pointer <{ {{.*}}, i32 6{{[0-9][0-9]}} }>
// CHECK-sans-workaround-SAME: = internal {{.*}} %swift.async_func_pointer <{ {{.*}}, i32 {{[0-9][0-9]}} }>

// CHECK: [[ASYNC_LET_ENTRY_BAR:@"\$s32async_let_back_deploy_workaround3bar1x1yS2i_SitYaFSiyYaYbcfu_Tu"]]
// CHECK-with-workaround-SAME: = internal {{.*}} %swift.async_func_pointer <{ {{.*}}, i32 6{{[0-9][0-9]}} }>
// CHECK-sans-workaround-SAME: = internal {{.*}} %swift.async_func_pointer <{ {{.*}}, i32 {{[0-9][0-9]}} }>

// CHECK-LABEL: define {{.*}}3foo
// CHECK: swift_asyncLet_begin{{.*}}[[ASYNC_LET_ENTRY_FOO]]
public func foo(x: Int, y: Int) async -> Int {
    async let z = x + y
    return await z
}

@_silgen_name("bar_work") func bar_work() -> Int

// CHECK-LABEL: define {{.*}}3bar
// CHECK: swift_asyncLet_begin{{.*}}[[ASYNC_LET_ENTRY_BAR]]
public func bar(x: Int, y: Int) async -> Int {
    async let z = bar_work()
    return await z
}
