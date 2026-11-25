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

// An inlined exactly-once closure that consumes a noncopyable capture and is
// called on either of two paths.

struct Resource: ~Copyable {
  let tag: String
  init(_ tag: String) { self.tag = tag }
  deinit { print("Resource(\(tag)) deinit") }
}

@inline(never)
func useResource(_ r: consuming Resource) { print("Resource(\(r.tag)) used") }

@inline(__always)
func callOnEitherPath(_ first: Bool, _ body: @once () -> Void) {
  if first {
    print("first path")
    body()
  } else {
    print("second path")
    body()
  }
}

@inline(never)
func testCalledOnEitherPath(_ first: Bool, _ tag: String) {
  let r = Resource(tag)
  callOnEitherPath(first) { useResource(r) }
}

testCalledOnEitherPath(true, "either first")
// CHECK-NEXT: first path
// CHECK-NEXT: Resource(either first) used
// CHECK-NEXT: Resource(either first) deinit
testCalledOnEitherPath(false, "either second")
// CHECK-NEXT: second path
// CHECK-NEXT: Resource(either second) used
// CHECK-NEXT: Resource(either second) deinit

// An exactly-once closure's context borrows a `borrowing` parameter that it
// captures, so the call doesn't destroy the parameter.

final class Tracker {
  let tag: String
  init(_ tag: String) { self.tag = tag }
  deinit { print("Tracker(\(tag)) deinit") }
}

func callOnce(_ body: @once () -> Void) { body() }

@inline(never)
func testBorrowingParameterCapture(_ t: borrowing Tracker) {
  callOnce { print("Tracker(\(t.tag)) captured") }
  print("Tracker(\(t.tag)) still alive")
}

// CHECK-NEXT: Tracker(borrowing) captured
// CHECK-NEXT: Tracker(borrowing) still alive
// CHECK-NEXT: Tracker(borrowing) deinit
do {
  let t = Tracker("borrowing")
  testBorrowingParameterCapture(t)
}

// An exactly-once closure that consumes a Copyable capture consumes a copy of
// it, so the captured value lives until the end of its own scope.

@inline(never)
func sinkTracker(_ t: consuming Tracker) { print("Tracker(\(t.tag)) consumed") }

@inline(never)
func testConsumedCopyableCapture(_ tag: String, explicitly: Bool) {
  let t = Tracker(tag)
  if explicitly {
    callOnce { sinkTracker(consume t) }
  } else {
    callOnce { sinkTracker(t) }
  }
  print("Tracker(\(t.tag)) still alive")
}

// CHECK-NEXT: Tracker(implicit) consumed
// CHECK-NEXT: Tracker(implicit) still alive
// CHECK-NEXT: Tracker(implicit) deinit
testConsumedCopyableCapture("implicit", explicitly: false)
// CHECK-NEXT: Tracker(explicit) consumed
// CHECK-NEXT: Tracker(explicit) still alive
// CHECK-NEXT: Tracker(explicit) deinit
testConsumedCopyableCapture("explicit", explicitly: true)
