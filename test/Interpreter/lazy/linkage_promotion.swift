// REQUIRES: OS=macosx
// REQUIRES: swift_interpreter
// REQUIRES: swift_feature_LazyImmediate
// RUN: %target-jit-run %s -enable-experimental-feature LazyImmediate | %FileCheck %s

// Tests that the linkage of private symbols is
// promoted to hidden external, allowing
// single-function compilation of non-public symbols.

fileprivate func foo() {
  print("foo")
}

func bar() {
  foo()
  print("bar")
}

// CHECK: foo
// CHECK-NEXT: bar
bar()
