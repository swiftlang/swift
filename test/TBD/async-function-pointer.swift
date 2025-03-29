// REQUIRES: VENDOR=apple
// REQUIRES: concurrency
// RUN: %target-swift-frontend -emit-ir %s  -target %target-swift-5.1-abi-triple -validate-tbd-against-ir=all -module-name test | %FileCheck %s

// CHECK: @barTu = global %swift.async_func_pointer
@_silgen_name("bar")
public func foo() async {}

// CHECK: @"$s4test1CC1f33_295642D23064661A21CD592AD781409CLLyyYaFTu" = global %swift.async_func_pointer 

open class C {
  private func f() async { }
}
