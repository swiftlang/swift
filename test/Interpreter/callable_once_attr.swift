// RUN: %target-run-simple-swift(-enable-experimental-feature OnceFunctions) | %FileCheck %s
// RUN: %target-run-simple-swift(-enable-experimental-feature OnceFunctions -O) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_OnceFunctions

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

func consume(_ f: @callable(once) (Payload) -> Void, _ value: Payload) {
  f(value)
}

func consumeEscaping(_ f: @escaping @callable(once) (Payload) -> Void, _ value: Payload) {
  f(value)
}

func dontConsume(_ f: @callable(once) (Payload) -> Void) { /* never called */ }

func dontConsumeEscaping(_ f: @escaping @callable(once) (Payload) -> Void) { /* never called */ }

func makeClosure(_ tag: String) -> @callable(once) () -> Void {
  return { print("called \(tag)") }
}

func callIt(_ f: @callable(once) () -> Void) {
  f()
}

// An at-most-once closure invoked directly.
func testDirectCall() {
  let f = makeClosure("direct")
  f()
}

// CHECK: called direct
testDirectCall()

// Passing an at-most-once value through a parameter and calling it there.
func testPassThrough() {
  callIt(makeClosure("passthrough"))
}

// CHECK-NEXT: called passthrough
testPassThrough()

// An at-most-once value captured (moved) into another at-most-once
// closure at formation time, then invoked through the wrapper.
func testWrappedCapture() {
  let f = makeClosure("wrapped")
  let g = { @callable(once) in f() }
  g()
}

// CHECK-NEXT: called wrapped
testWrappedCapture()

// A captured `var` is moved into the closure at formation; reassigning the
// var afterward doesn't affect what the closure already captured, and the
// new value assigned to `f` is independently callable.
func testVarCaptureReassignedAfterFormation() {
  var f = makeClosure("original")
  let g = { @callable(once) in f() }
  f = makeClosure("reassigned")
  g()
  f()
}

// CHECK-NEXT: called original
// CHECK-NEXT: called reassigned
testVarCaptureReassignedAfterFormation()

// Nested at-most-once closures: the outer closure's capture (`inner`)
// itself captured `f` at its own formation time.
func testNestedClosures() {
  let f = makeClosure("nested")
  let inner = { @callable(once) in f() }
  let outer = { @callable(once) in inner() }
  outer()
}

// CHECK-NEXT: called nested
testNestedClosures()

// An at-most-once closure with a consuming capture that is never called
// must still destroy the capture when it goes out of scope.
func testConsumingCaptureNeverCalled() {
  let r = Resource("neverCalled")
  let g = { @callable(once) in r.use() }
  _ = g
}

// CHECK-NEXT: Resource(neverCalled) deinit
testConsumingCaptureNeverCalled()

// An at-most-once closure with a consuming capture that *is* called: the
// capture must be destroyed exactly once, by the use inside the closure body,
// not a second time when the closure's own context is torn down.
func testConsumingCaptureCalled() {
  let r = Resource("called")
  let g = { @callable(once) in r.use() }
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
  let g = { @callable(once) in
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
  let g = { @callable(once) in r.use() }
  g()
}

// CHECK-NEXT: EmptyResource used
// CHECK-NEXT: EmptyResource deinit
testEmptyConsumingCaptureCalled()

func testEmptyConsumingCaptureNeverCalled() {
  let r = EmptyResource()
  let g = { @callable(once) in r.use() }
  _ = g
}

// CHECK-NEXT: EmptyResource deinit
testEmptyConsumingCaptureNeverCalled()

// The same scenarios via a parameter (not just a local `let`), exercising
// the at-most-once parameter binding path rather than closure formation.
func acceptsCalledAtMostOnce(_ f: @callable(once) () -> Void) { /* never called */ }

func testParameterNeverCalled() {
  let r = Resource("param")
  acceptsCalledAtMostOnce { r.use() }
}

// CHECK-NEXT: Resource(param) deinit
testParameterNeverCalled()

func acceptsAndCallsCalledAtMostOnce(_ f: @callable(once) () -> Void) {
  f()
}

func testParameterCalled() {
  let r = Resource("paramCalled")
  acceptsAndCallsCalledAtMostOnce { r.use() }
}

// CHECK-NEXT: Resource(paramCalled) used
// CHECK-NEXT: Resource(paramCalled) deinit
testParameterCalled()

// Test non-escaping at-most-once that is stack promoted.

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

// A stack-promoted closure that consumes one noncopyable capture and only
// borrows another one owns the first, but the enclosing function still owns the
// second, so calling or dropping the closure must not destroy it.

func testStackPromotedMixedNoncopyableCapturesCalled() {
  let r = Resource("mixedCalled")
  let v = BorrowableValue("mixedCalled")
  callIt {
    v.peek()
    r.use()
  }
}

// CHECK-NEXT: BorrowableValue(mixedCalled) peek
// CHECK-NEXT: Resource(mixedCalled) used
// CHECK-NEXT: Resource(mixedCalled) deinit
// CHECK-NEXT: BorrowableValue(mixedCalled) deinit
testStackPromotedMixedNoncopyableCapturesCalled()

func testStackPromotedMixedNoncopyableCapturesNeverCalled() {
  let r = Resource("mixedNeverCalled")
  let v = BorrowableValue("mixedNeverCalled")
  acceptsCalledAtMostOnce {
    v.peek()
    r.use()
  }
}

// CHECK-NEXT: Resource(mixedNeverCalled) deinit
// CHECK-NEXT: BorrowableValue(mixedNeverCalled) deinit
testStackPromotedMixedNoncopyableCapturesNeverCalled()

// Everything below duplicates the consuming-capture scenarios above for
// escaping at-most-once closures, to make sure escaping closures don't
// behave any differently from noescape ones.

func acceptsCalledAtMostOnceEscaping(_ f: @escaping @callable(once) () -> Void) { /* never called */ }

func acceptsAndCallsCalledAtMostOnceEscaping(_ f: @escaping @callable(once) () -> Void) {
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
// `partial_apply [called_at_most_once]` can attach at-most-once semantics to
// the result.
func makeCalledAtMostOnce(_ f: @escaping (Payload) -> Void) -> @callable(once) (Payload) -> Void {
  return identity(f)
}

// A generic function's own body performs the escaping-to-at-most-once
// conversion directly on its abstract parameter, sharing one thunk across
// every instantiation of `T`.
func genericMakeCalledAtMostOnce<T>(_ f: @escaping (T) -> Void) -> @callable(once) (T) -> Void {
  return f
}

// A thunked conversion landing in an (implicitly noescape) at-most-once
// parameter still runs correctly.
func testCalledThroughThunk() {
  consume(makeCalledAtMostOnce { print("called \($0.tag)") }, Payload(tag: "direct"))
}

// CHECK: called direct
testCalledThroughThunk()

// The same conversion landing in an escaping at-most-once parameter.
func testCalledThroughThunkEscaping() {
  consumeEscaping(makeCalledAtMostOnce { print("called \($0.tag)") }, Payload(tag: "escaping"))
}

// CHECK-NEXT: called escaping
testCalledThroughThunkEscaping()

// Calling a generically-produced at-most-once closure at a concrete type
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
func specializedCalledAtMostOnce(_ fn: @callable(once) () -> Void) {
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

// A generic noncopyable value is address-only, so a closure that consumes it
// captures it indirectly. The closure's context owns the value, which is
// destroyed exactly once, whether or not the closure is called and whether or
// not it escapes.
struct GenericResource<T>: ~Copyable {
  let value: T
  let tag: String
  init(_ value: T, _ tag: String) { self.value = value; self.tag = tag }
  deinit { print("GenericResource(\(tag)) deinit") }
  consuming func use() { print("GenericResource(\(tag)) used") }
  mutating func mutate() { print("GenericResource(\(tag)) mutated") }
}

@inline(never)
func testGenericConsumingCapture<T>(_ value: T) {
  let r = GenericResource(value, "called")
  callIt {
    r.use()
  }
}

// CHECK-NEXT: GenericResource(called) used
// CHECK-NEXT: GenericResource(called) deinit
testGenericConsumingCapture(1)

@inline(never)
func testGenericConsumingCaptureNeverCalled<T>(_ value: T) {
  let r = GenericResource(value, "unused")
  dontConsume { (_: Payload) in
    r.use()
  }
}

// CHECK-NEXT: GenericResource(unused) deinit
testGenericConsumingCaptureNeverCalled(1)

@inline(never)
func testGenericConsumingCaptureEscaping<T>(_ value: T) {
  let r = GenericResource(value, "escaping")
  consumeEscaping({ (_: Payload) in r.use() }, Payload(tag: "x"))
}

// CHECK-NEXT: GenericResource(escaping) used
// CHECK-NEXT: GenericResource(escaping) deinit
testGenericConsumingCaptureEscaping(1)

@inline(never)
func testGenericConsumingCaptureEscapingNeverCalled<T>(_ value: T) {
  let r = GenericResource(value, "escaping unused")
  dontConsumeEscaping { (_: Payload) in r.use() }
}

// CHECK-NEXT: GenericResource(escaping unused) deinit
testGenericConsumingCaptureEscapingNeverCalled(1)

// A closure that mutates a generic noncopyable value captures it inout, so the
// closure's context doesn't own the value. The caller still owns it, and
// destroys it exactly once, whether or not the closure is called.
@inline(never)
func mutateGenericInoutCapture<T>(_ r: inout GenericResource<T>) {
  callIt {
    r.mutate()
  }
}

@inline(never)
func testGenericInoutCapture<T>(_ value: T) {
  var r = GenericResource(value, "inout")
  mutateGenericInoutCapture(&r)
  r.use()
}

// CHECK-NEXT: GenericResource(inout) mutated
// CHECK-NEXT: GenericResource(inout) used
// CHECK-NEXT: GenericResource(inout) deinit
testGenericInoutCapture(1)

@inline(never)
func testGenericInoutCaptureNeverCalled<T>(_ value: T) {
  var r = GenericResource(value, "inout unused")
  dontConsume { (_: Payload) in
    r.mutate()
  }
  r.use()
}

// CHECK-NEXT: GenericResource(inout unused) used
// CHECK-NEXT: GenericResource(inout unused) deinit
testGenericInoutCaptureNeverCalled(1)

// A closure that consumes one generic noncopyable value and mutates another
// owns only the consumed value.
@inline(never)
func testGenericConsumingAndInoutCaptures<T>(_ value: T) {
  let consumed = GenericResource(value, "mixed consumed")
  var mutated = GenericResource(value, "mixed inout")
  callIt {
    consumed.use()
    mutated.mutate()
  }
  mutated.use()
}

// CHECK-NEXT: GenericResource(mixed consumed) used
// CHECK-NEXT: GenericResource(mixed consumed) deinit
// CHECK-NEXT: GenericResource(mixed inout) mutated
// CHECK-NEXT: GenericResource(mixed inout) used
// CHECK-NEXT: GenericResource(mixed inout) deinit
testGenericConsumingAndInoutCaptures(1)

@inline(never)
func testGenericConsumingAndInoutCapturesNeverCalled<T>(_ value: T) {
  let consumed = GenericResource(value, "mixed consumed unused")
  var mutated = GenericResource(value, "mixed inout unused")
  dontConsume { (_: Payload) in
    consumed.use()
    mutated.mutate()
  }
  mutated.use()
}

// CHECK-NEXT: GenericResource(mixed consumed unused) deinit
// CHECK-NEXT: GenericResource(mixed inout unused) used
// CHECK-NEXT: GenericResource(mixed inout unused) deinit
testGenericConsumingAndInoutCapturesNeverCalled(1)

@_transparent
func transparentAnd(
  _ lhs: Bool, _ rhs: @autoclosure @callable(once) () -> Bool
) -> Bool {
  lhs ? rhs() : false
}

func isPositive(_ t: Tracker, _ value: Int) -> Bool {
  print("checking \(t.tag)")
  return value > 0
}

func testInlinedAutoclosure(_ flag: Bool, _ tag: String) {
  let t = Tracker(tag)
  print(transparentAnd(flag, isPositive(t, 1)))
}

// CHECK-NEXT: checking inlined called
// CHECK-NEXT: true
// CHECK-NEXT: Tracker(inlined called) deinit
testInlinedAutoclosure(true, "inlined called")

// CHECK-NEXT: false
// CHECK-NEXT: Tracker(inlined never called) deinit
testInlinedAutoclosure(false, "inlined never called")

@_transparent
func transparentRebound(_ fn: @callable(once) () -> Void) {
  let g = fn
  g()
}

@_transparent
func transparentForwarded(_ fn: @callable(once) () -> Void) {
  transparentRebound(fn)
}

@_transparent
func transparentDiscard(_ fn: @callable(once) () -> Void) {
  _ = consume fn
}

func testInlinedRebinding() {
  let t = Tracker("rebound")
  transparentRebound { print("using \(t.tag)") }
}

// CHECK-NEXT: using rebound
// CHECK-NEXT: Tracker(rebound) deinit
testInlinedRebinding()

func testInlinedForwarding() {
  let t = Tracker("forwarded")
  transparentForwarded { print("using \(t.tag)") }
}

// CHECK-NEXT: using forwarded
// CHECK-NEXT: Tracker(forwarded) deinit
testInlinedForwarding()

func testInlinedDiscard() {
  let r = Resource("discarded")
  transparentDiscard { r.use() }
}

// CHECK-NEXT: Resource(discarded) deinit
testInlinedDiscard()

func genericCallAtMostOnce<R>(_ fn: @callable(once) () -> R) -> R { fn() }

func makeTag() -> String { "thunk" }

// CHECK-NEXT: thunk
print(genericCallAtMostOnce(makeTag))
