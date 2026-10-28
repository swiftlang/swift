// RUN: %target-swift-frontend %s \
// RUN: -emit-sil -target %target-swift-5.1-abi-triple \
// RUN: -enable-experimental-feature OnceFunctions \
// RUN: -verify

// REQUIRES: swift_feature_OnceFunctions

func makeClosure() -> @callable(once) () -> Void {
  return {}
}

func testNeverCalled(_ f: @callable(once) () -> Void) {
  // Ok (at-most-once has "at most" semantics
}

func testCallAtMostOnce(_ f: @callable(once) () -> Void) {
  f()
}

func testCalledInOneBranchOnly(cond: Bool, _ f: @callable(once) () -> Void) {
  if cond {
    f() // Ok (still "at most once")
  }
}

func testCalledInBothBranches(cond: Bool, _ f: @callable(once) () -> Void) {
  if cond {
    f()
  } else {
    f()
  }
}

func testLocal() {
  let f: @callable(once) () -> Void = makeClosure()
  f() // Ok
}

func testMoveThenCall(_ f: @callable(once) () -> Void) {
  let g = f
  g()
}

func testMoveThenCallConditional(cond: Bool, _ f: @callable(once) () -> Void) {
  let g = f
  // expected-error@-1 {{'g' consumed more than once}}

  g() // expected-note {{consumed here}}

  if cond {
    g() // expected-note {{consumed again here}}
  }
}

func testImmediateCallOnReturnValue() {
  makeClosure()() // Ok
}

func testDoubleCall(_ f: @callable(once) () -> Void) { // expected-error {{'f' consumed more than once}}
  f() // expected-note {{consumed here}}
  f() // expected-note {{consumed again here}}
}

func testCalledInLoop(_ f: @callable(once) () -> Void) { // expected-error {{'f' consumed in a loop}}
  for _ in 0..<10 {
    f() // expected-note {{consumed here}}
  }
}

func testWrappedInLocalClosure(_ f: @escaping @callable(once) () -> Void) {
  let g = { @callable(once) [f] in f() }
  g() // Ok
}

func testWrappedInLocalClosureImplicitly(_ f: @escaping @callable(once) () -> Void) {
  let g = { @callable(once) in f() }
  g()
}

func testWrappedInLocalClosureAndReassigned1(_ f: @escaping @callable(once) () -> Void) {
  var g = { @callable(once) in f() }
  g()
  g = { }
  g()
}

func testWrappedInLocalClosureAndReassigned2(_ f: @escaping @callable(once) () -> Void) {
  let g = { @callable(once) in f() }
  f = { }
  g()
}

func testWrappedInLocalClosureAndReassigned3(_ f: @escaping @callable(once) () -> Void) {
  let g = { @callable(once) in f() }
  f = { }
  g()
  f()
}

func testWrappedInLocalClosureAndReassignedMultiConsume(_ f: @escaping @callable(once) () -> Void) { // expected-error {{'f' consumed more than once}}
  let g = { @callable(once) in f() } // expected-note {{consumed here}}
  f() // expected-note {{consumed again here}}
  f = { }
  g() // Ok
  f() // Ok
}

func passThroughParam(fn: @callable(once) () -> Void) {
  testCallAtMostOnce(fn) // Ok
}

func passThroughEscapingParam(fn: @escaping @callable(once) () -> Void) {
  testCallAtMostOnce(fn) // Ok
}

func testCallAtMostOnceThenForward(_ f: @callable(once) () -> Void) { // expected-error {{'f' consumed more than once}}
  f() // expected-note {{consumed here}}
  testCallAtMostOnce(f) // expected-note {{consumed again here}}
}

func passThroughClosureOptional(fn: consuming (@callable(once) () -> Void)?) {
  guard let fn else { return }
  fn()
}

func passThroughClosureOptionalAndUnwrap(fn: consuming (@callable(once) () -> Void)?) { // expected-error {{'fn' consumed more than once}}
  fn?() // expected-note {{consumed here}}
  guard let fn else { return } // expected-note {{consumed again here}}
  _ = fn
}

func testEarlyReturnBeforeCall(cond: Bool, _ f: @escaping @callable(once) () -> Void) {
  if cond { return }
  f()
}

func passConditionalThroughParamThenCall(cond: Bool, fn: @callable(once) () -> Void) { // expected-error {{'fn' consumed more than once}}
  if cond {
    testCallAtMostOnce(fn) // expected-note {{consumed here}}
  }
  fn() // expected-note {{consumed again here}}
}

func localCaptureThenCall() {
  let fn: @callable(once) () -> Void = { } // expected-error {{'fn' consumed more than once}}
  _ = { @callable(once) [fn] in testCallAtMostOnce(fn) } // expected-note {{consumed here}}
  fn() // expected-note {{consumed again here}}

  let other: @callable(once) () -> Void = { } // expected-error {{'other' consumed more than once}}
  _ = { @callable(once) in testCallAtMostOnce(other) } // expected-note {{consumed here}}
  other() // expected-note {{consumed again here}}
}

func captureInRegularClosure(
  f: @escaping @callable(once) () -> Void,
  g: @escaping @callable(once) () -> Void,
  optF: consuming (@callable(once) () -> Void)? = nil
) {
  _ = { [f] in f() } // expected-error {{non-'Copyable' 'f' cannot be consumed when captured by an escaping closure or borrowed by a non-'Escapable' type}}
  _ = { g() } // expected-error {{non-'Copyable' 'g' cannot be consumed when captured by an escaping closure or borrowed by a non-'Escapable' type}}

  guard let optF else { return }
  _ = { @callable(once) in optF() } // Ok
}

func testTwoIndependentParams(_ f: @callable(once) () -> Void, _ g: @callable(once) () -> Void) {
  f()
  g()
}

func testTwoIndependentParamsOneDoubled(_ f: @callable(once) () -> Void, _ g: @callable(once) () -> Void) { // expected-error {{'g' consumed more than once}}
  f()
  g() // expected-note {{consumed here}}
  g() // expected-note {{consumed again here}}
}

func testSwitchEachCaseCallsAtMostOnce(_ x: Int, _ f: @callable(once) () -> Void) {
  switch x {
  case 0:
    f()
  default:
    f()
  }
}

// Capturing `f` into `a` consumes it right there; calling `f()` again directly
// afterward must be caught locally, without any interprocedural reasoning
// about what `a`'s body does with its capture.
func testParamCaptureThenCallDirectly(f: @escaping @callable(once) () -> Void) { // expected-error {{'f' consumed more than once}}
  let a = { @callable(once) in f() } // expected-note {{consumed here}}
  f() // expected-note {{consumed again here}}
  a()
}

// Same value captured by two different at-most-once closures without an
// intervening reassignment - only the first capture may consume it.
func testCapturedByTwoClosures(_ f: @escaping @callable(once) () -> Void) { // expected-error {{'f' consumed more than once}}
  let a = { @callable(once) in f() } // expected-note {{consumed here}}
  let b = { @callable(once) in f() } // expected-note {{consumed again here}}
  a()
  b()
}

// ... but capturing into two closures along mutually exclusive paths is fine.
func testCapturedByTwoClosuresMutuallyExclusive(_ f: @escaping @callable(once) () -> Void, cond: Bool) {
  if cond {
    let a = { @callable(once) in f() }
    a()
  } else {
    let b = { @callable(once) in f() }
    b()
  }
}

// A closure capturing multiple independent at-most-once values may call
// each of them once.
func testClosureCapturesTwoIndependentValues(f: @escaping @callable(once) () -> Void, g: @escaping @callable(once) () -> Void) {
  let a = { @callable(once) in
    f()
    g()
  }
  a()
}

// The "at most once" checking still applies *inside* the closure body to its
// own captures.
func testClosureCapturesAndDoubleCallsOneOfThem(f: @escaping @callable(once) () -> Void, g: @callable(once) () -> Void) { // expected-error 2 {{'f' consumed more than once}}
  let a = { @callable(once) in
    f() // expected-note 2 {{consumed here}}
    f() // expected-note 2 {{consumed again here}}
  }
  a()
}

// An at-most-once value can also be captured alongside ordinary (copyable)
// values without disturbing their capture kind.
func testMixedCaptureKinds(_ f: @escaping @callable(once) () -> Void, x: Int) {
  let g = { @callable(once) in
    print(x)
    f()
  }
  g()
}

// Nesting: an outer at-most-once closure capturing an inner one that
// itself captures the original value.
func testNestedCalledAtMostOnceClosures(_ f: @escaping @callable(once) () -> Void) {
  let inner = { @callable(once) in f() }
  let outer = { @callable(once) in inner() }
  outer()
}

func testNestedCalledAtMostOnceClosuresDouble(_ f: @escaping @callable(once) () -> Void) {
  let inner = { @callable(once) in f() }
  let outer = { @callable(once) in inner() }
  // expected-error@-1 {{'outer' consumed more than once}}
  outer() // expected-note {{consumed here}}
  outer() // expected-note {{consumed again here}}
}

// Returning a closure that captures an at-most-once value: the capture is
// consumed where the closure literal is formed, not where it's later called.
func makeWrapper(_ f: @escaping @callable(once) () -> Void) -> @callable(once) () -> Void {
  return { @callable(once) in f() }
}

func testUseWrapper(_ f: @escaping @callable(once) () -> Void) {
  let w = makeWrapper(f)
  w()
}

// A captured `var` follows the same "consumed at formation" rule as a `let`;
// reassigning it afterward is ordinary reinitialization, and using the new
// value again later is fine.
func testVarCaptureReassignedAfterFormation(_ f: @escaping @callable(once) () -> Void) {
  var f = f
  let g = { @callable(once) in f() }
  f = makeClosure()
  g()
  f()
}

// Without the reassignment, using `f` again after it was captured is a
// double consumption, exactly as it would be for a `let`.
func testVarCaptureUsedAgainWithoutReassignment(_ f: @escaping @callable(once) () -> Void) {
  var f = f // expected-warning {{variable 'f' was never mutated; consider changing to 'let' constant}}
  // expected-error@-1 {{'f' consumed more than once}}
  let g = { @callable(once) in f() } // expected-note {{consumed here}}
  g()
  f() // expected-note {{consumed again here}}
}

// Explicit capture-list renaming (`[g2 = f]`) goes through the same
// consuming-capture path as a plain `[f]` capture.
func testCaptureListRename(_ f: @escaping @callable(once) () -> Void) {
  let g = { @callable(once) [g2 = f] in g2() }
  g()
}

// Use at-most-once as a property type, the parameter is implicitly `consuming`.
do {
  struct S1: ~Copyable {
    let operation: @callable(once) () -> Void
    
    init(operation: @escaping @callable(once) () -> Void) {
      // okay; the parameter is implicitly 'consuming'
      self.operation = operation
    }
    
    consuming func call() {
      operation()
    }
  }

  struct S2: ~Copyable {
    let operation: @callable(once) () -> Void
    
    init(operation: @escaping @callable(once) () -> Void) {
      self.operation = operation
    }

    deinit { // expected-note {{deinitializer declared here}}
    }

    consuming func call() {
      operation() // expected-error {{cannot partially consume 'self' when it has a deinitializer}}
    }
  }
}

// FIXME: [called-once] Passing an `inout` value of at-most-once function type
// to a generic `inout T` parameter is rejected, eventhough nothing consumes it.
func genericInout<T: ~Copyable>(_ x: inout T) {}
func genericInoutOptional<T: ~Copyable>(_ x: inout T?) {}

func testGenericInout(_ f: inout @callable(once) () -> Void) { // expected-error {{'f' used after consume}}
  genericInout(&f) // expected-note {{consumed here}} expected-note {{used here}}
}

func testGenericInoutOptional(_ f: inout (@callable(once) () -> Void)?) { // expected-error {{'f' used after consume}}
  genericInoutOptional(&f) // expected-note {{consumed here}} expected-note {{used here}}
}

func testOptionalTake(_ f: inout (@callable(once) () -> Void)?) { // expected-error {{'f' used after consume}}
  _ = f.take() // expected-note {{consumed here}} expected-note {{used here}}
}

final class ClosureHolder {
  var f: (@callable(once) () -> Void)?

  func take() {
    _ = f.take() // expected-error {{cannot consume non-'Copyable' stored property 'self.f' of a class}}
  }
}

// MARK: - rethrows

struct RethrowsError: Error {}

func rethrowsCallAtMostOnce<R>(
  _ body: @callable(once) () throws -> R
) rethrows -> R? {
  Bool.random() ? try body() : nil
}

func rethrowsForwardAtMostOnce<R>(
  _ body: @callable(once) () throws -> R
) rethrows -> R? {
  try rethrowsCallAtMostOnce(body)
}

func rethrowsCallOptionalAtMostOnce(
  _ body: consuming (@callable(once) () throws -> Void)?
) rethrows {
  try body?()
}

func callRethrowsAtMostOnce() throws {
  _ = rethrowsCallAtMostOnce { 1 }
  _ = rethrowsForwardAtMostOnce { 1 }
  rethrowsCallOptionalAtMostOnce(nil)
  rethrowsCallOptionalAtMostOnce { }
  _ = try rethrowsForwardAtMostOnce { () throws -> Int in throw RethrowsError() }
}
