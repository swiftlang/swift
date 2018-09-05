// RUN: %target-swift-frontend -emit-sil %s | %FileCheck %s

import TensorFlow

public func test1() {
  func addSelf(_ x: Tensor<Float>) -> Tensor<Float> {
    return log(x)
  }
  _ = #gradient(addSelf)
}
