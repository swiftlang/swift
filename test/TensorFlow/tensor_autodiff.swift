// RUN: %target-swift-frontend -O -emit-sil %s | %FileCheck %s
// RUN: %target-swift-frontend -Xllvm -differentiation-use-vjp=false -O -emit-sil %s | %FileCheck %s

import TensorFlow

public func matsquare(_ x: Tensor<Float>) -> Tensor<Float> {
  return matmul(x, x)
}

public func test1() {
  _ = #gradient(matsquare)(Tensor([[1, 1], [1, 1]]))
}

// CHECK: @{{.*}}matsquare{{.*}}__grad_src_0_wrt_0

// SR-8709
public func selfmin(_ x: Tensor<Float>) -> Tensor<Float> {
  return min(x, x)
}

public func test2() {
  _ = #gradient(selfmin)(Tensor(1))
}

// CHECK: @{{.*}}selfmin{{.*}}__grad_src_0_wrt_0
