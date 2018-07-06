// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -Xllvm -tf-dump-graph -O -emit-sil %s -verify | %FileCheck %s
// REQUIRES: OS=macosx

import TensorFlow
import Foundation

// CHECK-LABEL: ---- ANALYSIS STATE FOR FUNCTION {{.*}}testDynamicMethodBranch
// CHECK:       bb0:
// CHECK:       [Send]    dynamic_method_br {{.*}}, bb1, bb2
// CHECK:       bb1:
// CHECK:       [Copy]    br bb3
// CHECK:       bb2:
// CHECK:       [Copy]    br bb3
// CHECK:       bb3:
// CHECK:           return
// CHECK-NEXT:  ---- END OF ANALYSIS STATE FOR FUNCTION

class X {
  @objc func f() {}
}
class Y {}
public func testDynamicMethodBranch(_ obj: AnyObject) {
  var b = Tensor<Float>(2.0)
  if let foo = obj.f {
    foo()
    b += 1.0
  }
  b -= 1.0
  _hostOp(b)
}
