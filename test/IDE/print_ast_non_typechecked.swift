class C {
  func foo(s: Int, w
}

// RUN: %swift-ide-test -print-ast-not-typechecked -source-filename %s | FileCheck %s -check-prefix=CHECK1
// CHECK1: func foo(s: Int, _: w)
