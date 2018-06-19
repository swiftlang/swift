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
  _ = baz()
}

main()

/// CHECK-LABEL: node_def {
/// CHECK:         op: "Identity"

/// CHECK-LABEL: node_def {
/// CHECK:         op: "Sin"

/// FIXME: Currently the performance inliner is not capturing inlining trace for
/// some reason. When that's fixed, add checks in this file.
///
/// Expected node names:
/// - Sin: op/main/baz/bar/sin
/// - Const: op/main/baz/bar/foo
