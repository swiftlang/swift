// REQUIRES: OS=macosx
// REQUIRES: swift_interpreter
// RUN: %target-jit-run %s -enable-experimental-feature LazyImmediate | %FileCheck %s

// REQUIRES: rdar118189728
// REQUIRES: swift_feature_LazyImmediate

// Tests that parsing of function bodies is deferred
// to runtime when the interpreter is invoked using
// experimental lazy compilation. Syntax errors in
// functions not executed at runtime should be ignored.

public func foo() {
  3 +
}

// Syntax error in `foo` should be ignored

// CHECK: hello
print("hello")

public func bar(_ b: Bool) {
  if b {
    print("bar")
  } else {
    foo()
  }
}

// Function `foo` with syntax error is not executed
// by bar, so the type error should again be ignored

// CHECK: bar
bar(true)

public func add(_ x: Int, _ y: Int, _ z: Int) -> Int {
  return x + y + z
}

public func sub(_ x: Int, _ y: Int) -> Int {
  return add(x,
}

// CHECK: 6
print(add(1, 2, 3))

