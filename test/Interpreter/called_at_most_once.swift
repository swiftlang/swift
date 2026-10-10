// RUN: %target-run-simple-swift(-enable-experimental-feature CalledAttribute) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_CalledAttribute

struct Resource: ~Copyable {
  let tag: String
  init(_ tag: String) { self.tag = tag }
  deinit { print("Resource(\(tag)) deinit") }
  consuming func use() { print("Resource(\(tag)) used") }
}

func identity<T>(_ f: @escaping (T) -> Void) -> (T) -> Void { f }

struct Payload {
  let tag: String
}

func consume(_ f: @called(atMostOnce) (Payload) -> Void, _ value: Payload) {
  f(value)
}

func consumeEscaping(_ f: @escaping @called(atMostOnce) (Payload) -> Void, _ value: Payload) {
  f(value)
}

func dontConsume(_ f: @called(atMostOnce) (Payload) -> Void) { /* never called */ }

func makeClosure(_ tag: String) -> @called(atMostOnce) () -> Void {
  return { print("called \(tag)") }
}

func callIt(_ f: @called(atMostOnce) () -> Void) {
  f()
}

// A `@called(atMostOnce)` closure invoked directly.
func testDirectCall() {
  let f = makeClosure("direct")
  f()
}

// CHECK: called direct
testDirectCall()

// Passing a `@called(atMostOnce)` value through a parameter and calling it there.
func testPassThrough() {
  callIt(makeClosure("passthrough"))
}

// CHECK-NEXT: called passthrough
testPassThrough()

// A `@called(atMostOnce)` value captured (moved) into another `@called(atMostOnce)`
// closure at formation time, then invoked through the wrapper.
func testWrappedCapture() {
  let f = makeClosure("wrapped")
  let g = { @called(atMostOnce) in f() }
  g()
}

// CHECK-NEXT: called wrapped
testWrappedCapture()

// A captured `var` is moved into the closure at formation; reassigning the
// var afterward doesn't affect what the closure already captured, and the
// new value assigned to `f` is independently callable.
func testVarCaptureReassignedAfterFormation() {
  var f = makeClosure("original")
  let g = { @called(atMostOnce) in f() }
  f = makeClosure("reassigned")
  g()
  f()
}

// CHECK-NEXT: called original
// CHECK-NEXT: called reassigned
testVarCaptureReassignedAfterFormation()

// Nested `@called(atMostOnce)` closures: the outer closure's capture (`inner`)
// itself captured `f` at its own formation time.
func testNestedClosures() {
  let f = makeClosure("nested")
  let inner = { @called(atMostOnce) in f() }
  let outer = { @called(atMostOnce) in inner() }
  outer()
}

// CHECK-NEXT: called nested
testNestedClosures()

// A `@called(atMostOnce)` closure with a consuming capture that is never called
// must still destroy the capture when it goes out of scope.
func testConsumingCaptureNeverCalled() {
  let r = Resource("neverCalled")
  let g = { @called(atMostOnce) in r.use() }
  _ = g
}

// CHECK-NEXT: Resource(neverCalled) deinit
testConsumingCaptureNeverCalled()

// A `@called(atMostOnce)` closure with a consuming capture that *is* called: the
// capture must be destroyed exactly once, by the use inside the closure body,
// not a second time when the closure's own context is torn down.
func testConsumingCaptureCalled() {
  let r = Resource("called")
  let g = { @called(atMostOnce) in r.use() }
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
  let g = { @called(atMostOnce) in
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
  let g = { @called(atMostOnce) in r.use() }
  g()
}

// CHECK-NEXT: EmptyResource used
// CHECK-NEXT: EmptyResource deinit
testEmptyConsumingCaptureCalled()

func testEmptyConsumingCaptureNeverCalled() {
  let r = EmptyResource()
  let g = { @called(atMostOnce) in r.use() }
  _ = g
}

// CHECK-NEXT: EmptyResource deinit
testEmptyConsumingCaptureNeverCalled()

// The same scenarios via a parameter (not just a local `let`), exercising
// the `@called(atMostOnce)` parameter binding path rather than closure formation.
func acceptsCalledAtMostOnce(_ f: @called(atMostOnce) () -> Void) { /* never called */ }

func testParameterNeverCalled() {
  let r = Resource("param")
  acceptsCalledAtMostOnce { r.use() }
}

// CHECK-NEXT: Resource(param) deinit
testParameterNeverCalled()

func acceptsAndCallsCalledAtMostOnce(_ f: @called(atMostOnce) () -> Void) {
  f()
}

func testParameterCalled() {
  let r = Resource("paramCalled")
  acceptsAndCallsCalledAtMostOnce { r.use() }
}

// CHECK-NEXT: Resource(paramCalled) used
// CHECK-NEXT: Resource(paramCalled) deinit
testParameterCalled()

// Test non-escaping `@called(atMostOnce)` that is stack promoted.

struct BorrowableValue: ~Copyable {
  let tag: String
  init(_ tag: String) { self.tag = tag }
  deinit { print("BorrowableValue(\(tag)) deinit") }
  borrowing func peek() { print("BorrowableValue(\(tag)) peek") }
}

func testStackPromotedClassCaptureCalled() {
  let t = Tracker("stackCalled")
  callIt { print("using \(t.tag)") }
}

// CHECK-NEXT: using stackCalled
// CHECK-NEXT: Tracker(stackCalled) deinit
testStackPromotedClassCaptureCalled()

func testStackPromotedClassCaptureNeverCalled() {
  let t = Tracker("stackNeverCalled")
  acceptsCalledAtMostOnce { print("using \(t.tag)") }
}

// CHECK-NEXT: Tracker(stackNeverCalled) deinit
testStackPromotedClassCaptureNeverCalled()

func testStackPromotedNoncopyableCaptureCalled() {
  let v = BorrowableValue("stackCalled")
  callIt { v.peek() }
}

// CHECK-NEXT: BorrowableValue(stackCalled) peek
// CHECK-NEXT: BorrowableValue(stackCalled) deinit
testStackPromotedNoncopyableCaptureCalled()

func testStackPromotedNoncopyableCaptureNeverCalled() {
  let v = BorrowableValue("stackNeverCalled")
  acceptsCalledAtMostOnce { v.peek() }
}

// CHECK-NEXT: BorrowableValue(stackNeverCalled) deinit
testStackPromotedNoncopyableCaptureNeverCalled()

// Everything below duplicates the consuming-capture scenarios above for
// `@escaping @called(atMostOnce)` closures, to make sure escaping closures don't
// behave any differently from noescape ones.

func acceptsCalledAtMostOnceEscaping(_ f: @escaping @called(atMostOnce) () -> Void) { /* never called */ }

func acceptsAndCallsCalledAtMostOnceEscaping(_ f: @escaping @called(atMostOnce) () -> Void) {
  f()
}

func testConsumingCaptureCalledEscaping() {
  let r = Resource("calledEscaping")
  acceptsAndCallsCalledAtMostOnceEscaping { r.use() }
}

// CHECK-NEXT: Resource(calledEscaping) used
// CHECK-NEXT: Resource(calledEscaping) deinit
testConsumingCaptureCalledEscaping()

func testConsumingCaptureNeverCalledEscaping() {
  let r = Resource("neverCalledEscaping")
  acceptsCalledAtMostOnceEscaping { r.use() }
}

// CHECK-NEXT: Resource(neverCalledEscaping) deinit
testConsumingCaptureNeverCalledEscaping()

func testMixedConsumingAndBorrowingCapturesEscaping() {
  let r = Resource("mixedEscaping")
  let t = Tracker("mixedEscaping")
  acceptsAndCallsCalledAtMostOnceEscaping {
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
  acceptsAndCallsCalledAtMostOnceEscaping { r.use() }
}

// CHECK-NEXT: EmptyResource used
// CHECK-NEXT: EmptyResource deinit
testEmptyConsumingCaptureCalledEscaping()

func testEmptyConsumingCaptureNeverCalledEscaping() {
  let r = EmptyResource()
  acceptsCalledAtMostOnceEscaping { r.use() }
}

// CHECK-NEXT: EmptyResource deinit
testEmptyConsumingCaptureNeverCalledEscaping()

// Passing a concrete closure through a generic passthrough forces a
// representation-changing reabstraction thunk before the final
// `partial_apply [called(atMostOnce)]` can attach `@called(atMostOnce)` to the result.
func makeCalledAtMostOnce(_ f: @escaping (Payload) -> Void) -> @called(atMostOnce) (Payload) -> Void {
  return identity(f)
}

// A generic function's own body performs the escaping-to-`@called(atMostOnce)`
// conversion directly on its abstract parameter, sharing one thunk across
// every instantiation of `T`.
func genericMakeCalledAtMostOnce<T>(_ f: @escaping (T) -> Void) -> @called(atMostOnce) (T) -> Void {
  return f
}

// A thunked conversion landing in an (implicitly noescape) `@called(atMostOnce)`
// parameter still runs correctly.
func testCalledThroughThunk() {
  consume(makeCalledAtMostOnce { print("called \($0.tag)") }, Payload(tag: "direct"))
}

// CHECK: called direct
testCalledThroughThunk()

// The same conversion landing in an `@escaping @called(atMostOnce)` parameter.
func testCalledThroughThunkEscaping() {
  consumeEscaping(makeCalledAtMostOnce { print("called \($0.tag)") }, Payload(tag: "escaping"))
}

// CHECK-NEXT: called escaping
testCalledThroughThunkEscaping()

// Calling a generically-produced `@called(atMostOnce)` closure at a concrete type
// forces a second thunk (bridging the concrete argument to the closure's
// abstract calling convention) at the call site itself.
func testGenericBodyConversion() {
  let f = genericMakeCalledAtMostOnce { (s: String) in print("generic called \(s)") }
  f("hello")
}

// CHECK-NEXT: generic called hello
testGenericBodyConversion()

// A capture carried through the thunked conversion is released exactly once
// when the closure is called.
func testCalledThroughThunkReleasesCapture() {
  let t = Tracker("used")
  let f = makeCalledAtMostOnce { (_: Payload) in
    print("using \(t.tag)")
  }
  consume(f, Payload(tag: "x"))
}

// CHECK-NEXT: using used
// CHECK-NEXT: Tracker(used) deinit
testCalledThroughThunkReleasesCapture()

// A capture carried through the thunked conversion is still released exactly
// once even when the closure is never called.
func testNeverCalledThroughThunkReleasesCapture() {
  let t = Tracker("unused")
  let f = makeCalledAtMostOnce { (_: Payload) in
    print("using \(t.tag)")
  }
  dontConsume(f)
}

// CHECK-NEXT: Tracker(unused) deinit
testNeverCalledThroughThunkReleasesCapture()

@inline(never)
func specializedCalledAtMostOnce(_ fn: @called(atMostOnce) () -> Void) {
  fn()
}

@inline(never)
func testClosureSpecializationConsumingCapture() {
  let r = Resource("specialized")
  specializedCalledAtMostOnce {
    r.use()
  }
}

// CHECK-NEXT: Resource(specialized) used
// CHECK-NEXT: Resource(specialized) deinit
testClosureSpecializationConsumingCapture()
