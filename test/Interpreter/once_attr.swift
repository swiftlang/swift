// RUN: %target-run-simple-swift(-enable-experimental-feature OnceFunctions) | %FileCheck %s
// RUN: %target-run-simple-swift(-enable-experimental-feature OnceFunctions -O -Xfrontend -sil-verify-all) | %FileCheck %s

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

// An inlined throwing exactly-once closure, called with `try`, whose `catch`
// block has another predecessor.

struct Failure: Error {}

@inline(__always)
func callThrowing(_ body: @once () throws -> Int) throws -> Int {
  try body()
}

@inline(never)
func testInlinedThrowing(_ shouldThrow: Bool, _ tag: String) {
  do {
    let r = try callThrowing {
      if shouldThrow { throw Failure() }
      return tag.count
    }
    print("inlined throwing: \(r)")
  } catch {
    print("inlined throwing: caught")
  }
}

testInlinedThrowing(true, "a")
// CHECK-NEXT: inlined throwing: caught
testInlinedThrowing(false, "bc")
// CHECK-NEXT: inlined throwing: 2

// A function reference passed for an exactly-once parameter that's captured by
// another exactly-once closure.

@inline(__always)
func callInlined<R>(_ x: Int, _ body: @once (Int) -> R) -> R {
  body(x)
}

@inline(never)
func forwardToInlined<R>(_ x: Int, _ body: @once (Int) -> R) -> R {
  callInlined(x) { body($0) }
}

func increment(_ x: Int) -> Int { x + 1 }

@inline(never)
func testCapturedFunctionReference(_ x: Int) -> Int {
  forwardToInlined(x, increment)
}

print("captured function reference: \(testCapturedFunctionReference(41))")
// CHECK-NEXT: captured function reference: 42
