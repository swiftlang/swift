// REQUIRES: VENDOR=apple
// REQUIRES: concurrency
// RUN: %target-swift-frontend -emit-ir %s -enable-experimental-concurrency -enable-experimental-async-handler -validate-tbd-against-ir=all -module-name test | %FileCheck %s

// CHECK: @"$s4test6testityyYaFTu" = hidden global %swift.async_func_pointer

@asyncHandler
public func testit() { }

// CHECK: @barTu = global %swift.async_func_pointer
@_silgen_name("bar")
public func foo() async {}

// CHECK: @"$s4test1CC1f33_295642D23064661A21CD592AD781409CLLyyYaFTu" = global %swift.async_func_pointer 

open class C {
  private func f() async { }
}
