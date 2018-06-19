// RUN: %target-swift-frontend -Xllvm -tf-dump-graph -O -emit-sil %s -verify | %FileCheck %s

import TensorFlow


func foo() -> Tensor<Float> {
  return Tensor(1)
}

func bar() -> Tensor<Float> {
  return sin(foo())
}

func baz() -> Tensor<Float> {
  return bar() + 1
}

func main() {
  baz()
}

main()

// CHECK-LABEL:      node_def {
// CHECK:            op: "Sin"
// CHECK-NEXT:       input: "op/main/baz/bar.
