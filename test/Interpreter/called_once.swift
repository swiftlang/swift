// RUN: %target-run-simple-swift(-enable-experimental-feature CalledAttribute) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_CalledAttribute

struct Resource: ~Copyable {
  let tag: String
  init(_ tag: String) { self.tag = tag }
  deinit { print("Resource(\(tag)) deinit") }
  consuming func use() { print("Resource(\(tag)) used") }
}

func makeClosure(_ tag: String) -> @called(once) () -> Void {
  return { print("called \(tag)") }
}

func callIt(_ f: @called(once) () -> Void) {
  f()
}

// A `@called(once)` closure invoked directly.
func testDirectCall() {
  let f = makeClosure("direct")
  f()
}

// CHECK: called direct
testDirectCall()

// Passing a `@called(once)` value through a parameter and calling it there.
func testPassThrough() {
  callIt(makeClosure("passthrough"))
}

// CHECK-NEXT: called passthrough
testPassThrough()

// A `@called(once)` value captured (moved) into another `@called(once)`
// closure at formation time, then invoked through the wrapper.
func testWrappedCapture() {
  let f = makeClosure("wrapped")
  let g = { @called(once) in f() }
  g()
}

// CHECK-NEXT: called wrapped
testWrappedCapture()

// A captured `var` is moved into the closure at formation; reassigning the
// var afterward doesn't affect what the closure already captured, and the
// new value assigned to `f` is independently callable.
func testVarCaptureReassignedAfterFormation() {
  var f = makeClosure("original")
  let g = { @called(once) in f() }
  f = makeClosure("reassigned")
  g()
  f()
}

// CHECK-NEXT: called original
// CHECK-NEXT: called reassigned
testVarCaptureReassignedAfterFormation()

// Nested `@called(once)` closures: the outer closure's capture (`inner`)
// itself captured `f` at its own formation time.
func testNestedClosures() {
  let f = makeClosure("nested")
  let inner = { @called(once) in f() }
  let outer = { @called(once) in inner() }
  outer()
}

// CHECK-NEXT: called nested
testNestedClosures()

// A `@called(once)` closure with a consuming capture that is never called
// must still destroy the capture when it goes out of scope.
func testConsumingCaptureNeverCalled() {
  let r = Resource("neverCalled")
  let g = { @called(once) in r.use() }
  _ = g
}

// CHECK-NEXT: Resource(neverCalled) deinit
testConsumingCaptureNeverCalled()

// A `@called(once)` closure with a consuming capture that *is* called: the
// capture must be destroyed exactly once, by the use inside the closure body,
// not a second time when the closure's own context is torn down.
func testConsumingCaptureCalled() {
  let r = Resource("called")
  let g = { @called(once) in r.use() }
  g()
}

// CHECK-NEXT: Resource(called) used
// CHECK-NEXT: Resource(called) deinit
testConsumingCaptureCalled()

// A closure mixing a consumed capture with a borrowed one: the borrowed
// capture must still be destroyed when the context is released, while the
// consumed one must be destroyed exactly once (by its use), not twice.
final class Tracker {
  let tag: String
  init(_ tag: String) { self.tag = tag }
  deinit { print("Tracker(\(tag)) deinit") }
}

func testMixedConsumingAndBorrowingCaptures() {
  let r = Resource("mixed")
  let t = Tracker("mixed")
  let g = { @called(once) in
    _ = t
    r.use()
  }
  g()
}

// CHECK-NEXT: Resource(mixed) used
// CHECK-NEXT: Resource(mixed) deinit
// CHECK-NEXT: Tracker(mixed) deinit
testMixedConsumingAndBorrowingCaptures()

// A zero-sized `~Copyable` consuming capture must still be destroyed
// (regression test: an empty capture must not be dropped from the
// context's layout, since it would then never be destroyed at all).
struct EmptyResource: ~Copyable {
  deinit { print("EmptyResource deinit") }
  consuming func use() { print("EmptyResource used") }
}

func testEmptyConsumingCaptureCalled() {
  let r = EmptyResource()
  let g = { @called(once) in r.use() }
  g()
}

// CHECK-NEXT: EmptyResource used
// CHECK-NEXT: EmptyResource deinit
testEmptyConsumingCaptureCalled()

func testEmptyConsumingCaptureNeverCalled() {
  let r = EmptyResource()
  let g = { @called(once) in r.use() }
  _ = g
}

// CHECK-NEXT: EmptyResource deinit
testEmptyConsumingCaptureNeverCalled()

// The same scenarios via a parameter (not just a local `let`), exercising
// the `@called(once)` parameter binding path rather than closure formation.
func acceptsCalledOnce(_ f: @called(once) () -> Void) { /* never called */ }

func testParameterNeverCalled() {
  let r = Resource("param")
  acceptsCalledOnce { r.use() }
}

// CHECK-NEXT: Resource(param) deinit
testParameterNeverCalled()

func acceptsAndCallsCalledOnce(_ f: @called(once) () -> Void) {
  f()
}

func testParameterCalled() {
  let r = Resource("paramCalled")
  acceptsAndCallsCalledOnce { r.use() }
}

// CHECK-NEXT: Resource(paramCalled) used
// CHECK-NEXT: Resource(paramCalled) deinit
testParameterCalled()

// Everything below duplicates the consuming-capture scenarios above for
// `@escaping @called(once)` closures, to make sure escaping closures don't
// behave any differently from noescape ones.

func acceptsCalledOnceEscaping(_ f: @escaping @called(once) () -> Void) { /* never called */ }

func acceptsAndCallsCalledOnceEscaping(_ f: @escaping @called(once) () -> Void) {
  f()
}

func testConsumingCaptureCalledEscaping() {
  let r = Resource("calledEscaping")
  acceptsAndCallsCalledOnceEscaping { r.use() }
}

// CHECK-NEXT: Resource(calledEscaping) used
// CHECK-NEXT: Resource(calledEscaping) deinit
testConsumingCaptureCalledEscaping()

func testConsumingCaptureNeverCalledEscaping() {
  let r = Resource("neverCalledEscaping")
  acceptsCalledOnceEscaping { r.use() }
}

// CHECK-NEXT: Resource(neverCalledEscaping) deinit
testConsumingCaptureNeverCalledEscaping()

func testMixedConsumingAndBorrowingCapturesEscaping() {
  let r = Resource("mixedEscaping")
  let t = Tracker("mixedEscaping")
  acceptsAndCallsCalledOnceEscaping {
    _ = t
    r.use()
  }
}

// CHECK-NEXT: Resource(mixedEscaping) used
// CHECK-NEXT: Resource(mixedEscaping) deinit
// CHECK-NEXT: Tracker(mixedEscaping) deinit
testMixedConsumingAndBorrowingCapturesEscaping()

func testEmptyConsumingCaptureCalledEscaping() {
  let r = EmptyResource()
  acceptsAndCallsCalledOnceEscaping { r.use() }
}

// CHECK-NEXT: EmptyResource used
// CHECK-NEXT: EmptyResource deinit
testEmptyConsumingCaptureCalledEscaping()

func testEmptyConsumingCaptureNeverCalledEscaping() {
  let r = EmptyResource()
  acceptsCalledOnceEscaping { r.use() }
}

// CHECK-NEXT: EmptyResource deinit
testEmptyConsumingCaptureNeverCalledEscaping()
