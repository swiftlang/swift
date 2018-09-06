// RUN: %target-swift-frontend -emit-sil %s | %FileCheck %s

import TensorFlow

public func test1() {
  func matsquare(_ x: Tensor<Float>) -> Tensor<Float> {
    return matmul(x, x)
  }
  _ = #gradient(matsquare)
}

// CHECK: @{{.*}}matsquare{{.*}}__grad_src_0_wrt_0
