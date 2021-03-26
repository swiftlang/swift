// REQUIRES: VENDOR=apple
// REQUIRES: concurrency
// RUN: %target-swift-frontend -emit-ir %s -enable-experimental-concurrency -enable-experimental-async-handler -validate-tbd-against-ir=all -module-name test | %FileCheck %s

// CHECK: @"$s4test6testityyYFTu" = hidden global %swift.async_func_pointer

@asyncHandler
public func testit() { }

// CHECK: @"$s4test1CC1f33_295642D23064661A21CD592AD781409CLLyyYFTu" = global %swift.async_func_pointer 

open class C {
  private func f() async { }
}
