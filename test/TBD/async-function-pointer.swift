// REQUIRES: VENDOR=apple 
// RUN: %target-swift-frontend -emit-ir %s -enable-experimental-concurrency -validate-tbd-against-ir=all -module-name test | %FileCheck %s

// CHECK: @"$s4test6testityyYFTu" = hidden global %swift.async_func_pointer

@asyncHandler
public func testit() { }
