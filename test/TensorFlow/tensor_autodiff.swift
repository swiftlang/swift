// RUN: %target-swift-frontend -O -emit-sil %s | %FileCheck %s

import TensorFlow

public func matsquare(_ x: Tensor<Float>) -> Tensor<Float> {
  return matmul(x, x)
}

public func test1() {
  _ = #gradient(matsquare)(Tensor([[1, 1], [1, 1]]))
}

// CHECK: @{{.*}}matsquare{{.*}}__grad_src_0_wrt_0
