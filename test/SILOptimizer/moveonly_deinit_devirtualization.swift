// RUN: %target-swift-frontend -sil-verify-all -emit-sil -O %s | %FileCheck %s

// REQUIRES: swift_in_compiler

public struct Outer<T> {}
extension Outer where T == UInt8 {
  public struct InnerNC: ~Copyable {
    var x: Int
    deinit {}
  }
}

// rdar://173803881 (~Copyable type in all-concrete constrained extension)
// The DeinitDevirtualization should not crash.
//
// CHECK-LABEL: sil @$s32moveonly_deinit_devirtualization24testConstrainedExtensionyyAA5OuterVAAs5UInt8VRszlE7InnerNCVyAF_GnF : $@convention(thin) (@owned Outer<UInt8>.InnerNC) -> () {
// CHECK-NOT: destroy
// CHECK-LABEL: } // end sil function '$s32moveonly_deinit_devirtualization24testConstrainedExtensionyyAA5OuterVAAs5UInt8VRszlE7InnerNCVyAF_GnF'
public func testConstrainedExtension(_ s: consuming Outer<UInt8>.InnerNC) {}
