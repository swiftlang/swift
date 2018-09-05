// RUN: %target-swift-frontend -emit-sil %s | %FileCheck %s
// XFAIL: *

import TensorFlow

public func test1() {
  func addSelf(_ x: Tensor<Float>) -> Tensor<Float> {
    return log(x)
  }
  _ = #gradient(addSelf)
}
