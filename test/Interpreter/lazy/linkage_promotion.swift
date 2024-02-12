// REQUIRES: OS=macosx
// REQUIRES: swift_interpreter
// RUN: %target-jit-run %s -enable-experimental-feature LazyImmediate | %FileCheck %s

// -enable-experimental-feature requires an asserts build
// REQUIRES: asserts

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
