// REQUIRES: OS=macosx
// REQUIRES: swift_interpreter
// REQUIRES: swift_feature_LazyImmediate
// RUN: %target-jit-run %s -enable-experimental-feature LazyImmediate | %FileCheck %s

// Tests that piecewise compilation works with global variables

let x = 1

// CHECK: 1
print(x)

fileprivate let y = 2

// CHECK: 2
print(y)

public let z = 3

// CHECK: 3
print(z)

var count = 0

func incr() -> Int {
  count += 1
  return count
}

// CHECK: 1
print(incr())

// CHECK: 2
print(incr())
