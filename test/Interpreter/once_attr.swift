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

// A plain closure converted to an exactly-once closure, which is called on one
// path and forwarded on another.

@inline(never)
func forwardNoninlined<R>(_ body: @once () -> R) -> R { body() }

@_optimize(none)
func callUnoptimized<R>(_ body: @once () -> R) -> R { body() }

@inline(__always)
func callOrForward<R>(_ direct: Bool, _ body: @once () -> R) -> R {
  if direct { return callUnoptimized(body) }
  return forwardNoninlined(body)
}

@inline(never)
func testCallOrForward(_ direct: Bool, _ body: () -> Int) -> Int {
  callOrForward(direct, body)
}

print("call or forward: \(testCallOrForward(true, { 1 })) \(testCallOrForward(false, { 2 }))")
// CHECK-NEXT: call or forward: 1 2

// Exactly-once closures with heap-allocated re-abstraction thunks, which are
// forwarded to a generic function.

@inline(__always)
func forwardToGeneric(_ body: @once () -> Int) -> Int {
  forwardNoninlined(body)
}

@inline(never)
func testForwardCapturingVar(_ y: Int) -> Int {
  var x = y
  let r = forwardToGeneric { x += 1; return x }
  return r + x
}

print("forward capturing var: \(testForwardCapturingVar(1))")
// CHECK-NEXT: forward capturing var: 4

@inline(never)
func forwardEscapingNoninlined<R>(_ body: @escaping @once () -> R) -> R { body() }

@inline(__always)
func forwardEscapingToGeneric(_ body: @escaping @once () -> Int) -> Int {
  forwardEscapingNoninlined(body)
}

final class Counter { var value = 1 }

@inline(never)
func testForwardEscaping(_ c: Counter) -> Int {
  forwardEscapingToGeneric { c.value }
}

print("forward escaping: \(testForwardEscaping(Counter()))")
// CHECK-NEXT: forward escaping: 1
