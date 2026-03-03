// REQUIRES: OS=macosx
// REQUIRES: swift_interpreter
// REQUIRES: swift_feature_LazyImmediate
// RUN: %target-jit-run %s -enable-experimental-feature LazyImmediate | %FileCheck %s

// Tests that type checking of function bodies is
// deferred to runtime when the interpreter is invoked
// using experimental lazy compilation. Type errors in
// functions not executed at runtime should be ignored.

public func foo() {
  3 + true
}

// Type error in `foo` should be ignored

// CHECK: hello
print("hello")

public func bar(_ b: Bool) {
  if b {
    print("bar")
  } else {
    foo()
  }
}

// Function `foo` with type error is not executed
// by bar, so the type error should again be ignored

// CHECK: bar
bar(true)

public func add(_ x: Int, _ y: Int, _ z: Int) -> Int {
  return x + y + z
}

public func sub(_ x: Int, _ y: Int) -> Int {
  return add(x, -y)
}

// CHECK: 6
print(add(1, 2, 3))
