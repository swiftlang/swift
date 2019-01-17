// RUN: %target-swift-frontend -O -emit-sil %s | %FileCheck %s

import TensorFlow

public func matsquare(_ x: Tensor<Float>) -> Tensor<Float> {
  return matmul(x, x)
}

public func test1() {
  _ = pullback(at: Tensor([[1, 1], [1, 1]]), in: matsquare)
}

// CHECK: @{{.*}}matsquare{{.*}}__vjp_src_0_wrt_0

// SR-8709
public func selfmin(_ x: Tensor<Float>) -> Tensor<Float> {
  return min(x, x)
}

public func test2() {
  _ = pullback(at: Tensor(1), in: selfmin)
}

// CHECK: @{{.*}}selfmin{{.*}}__vjp_src_0_wrt_0
