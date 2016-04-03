class C {
  func foo(_ s: Int, w
}

// RUN: %target-swift-ide-test -print-ast-not-typechecked -source-filename %s | FileCheck %s -check-prefix=CHECK1
// CHECK1: func foo(_ s: Int)
