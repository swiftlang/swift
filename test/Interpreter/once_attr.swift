// RUN: %target-run-simple-swift(-enable-experimental-feature OnceFunctions) | %FileCheck %s
// RUN: %target-run-simple-swift(-enable-experimental-feature OnceFunctions -O) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_OnceFunctions

// A generic function that can trap before it calls an exactly-once closure,
// which another function forwards to it.

@inline(__always)
func callChecked<R>(_ x: Int, _ body: @once (Int) -> R) -> R {
  if x < 0 { fatalError() }
  return body(x)
}

func forward(_ x: Int, _ body: @once (Int) -> Int) -> Int {
  callChecked(x, body)
}

@inline(never)
func testForwardingToTrappingFunction(_ x: Int, _ y: Int) -> Int {
  forward(x) { $0 + y }
}

print("forwarding: \(testForwardingToTrappingFunction(41, 1))")
// CHECK: forwarding: 42
