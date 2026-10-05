// RUN: %target-swift-frontend %s \
// RUN: -emit-sil -target %target-swift-5.1-abi-triple \
// RUN: -enable-experimental-feature CalledAttribute \
// RUN: -verify

// REQUIRES: swift_feature_CalledAttribute

func makeClosure() -> @called(atMostOnce) () -> Void {
  return {}
}

func testNeverCalled(_ f: @called(atMostOnce) () -> Void) {
  // Ok (`@called(atMostOnce)` has "at most" semantics
}

func testCallAtMostOnce(_ f: @called(atMostOnce) () -> Void) {
  f()
}

func testCalledInOneBranchOnly(cond: Bool, _ f: @called(atMostOnce) () -> Void) {
  if cond {
    f() // Ok (still "at most once")
  }
}

func testCalledInBothBranches(cond: Bool, _ f: @called(atMostOnce) () -> Void) {
  if cond {
    f()
  } else {
    f()
  }
}

func testLocal() {
  let f: @called(atMostOnce) () -> Void = makeClosure()
  f() // Ok
}

func testMoveThenCall(_ f: @called(atMostOnce) () -> Void) {
  let g = f
  g()
}

func testMoveThenCallConditional(cond: Bool, _ f: @called(atMostOnce) () -> Void) {
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

func testDoubleCall(_ f: @called(atMostOnce) () -> Void) { // expected-error {{'f' consumed more than once}}
  f() // expected-note {{consumed here}}
  f() // expected-note {{consumed again here}}
}

func testCalledInLoop(_ f: @called(atMostOnce) () -> Void) { // expected-error {{'f' consumed in a loop}}
  for _ in 0..<10 {
    f() // expected-note {{consumed here}}
  }
}

func testWrappedInLocalClosure(_ f: @escaping @called(atMostOnce) () -> Void) {
  let g = { @called(atMostOnce) [f] in f() }
  g() // Ok
}

func testWrappedInLocalClosureImplicitly(_ f: @escaping @called(atMostOnce) () -> Void) {
  let g = { @called(atMostOnce) in f() }
  g()
}

func testWrappedInLocalClosureAndReassigned1(_ f: @escaping @called(atMostOnce) () -> Void) {
  var g = { @called(atMostOnce) in f() }
  g()
  g = { }
  g()
}

func testWrappedInLocalClosureAndReassigned2(_ f: @escaping @called(atMostOnce) () -> Void) {
  let g = { @called(atMostOnce) in f() }
  f = { }
  g()
}

func testWrappedInLocalClosureAndReassigned3(_ f: @escaping @called(atMostOnce) () -> Void) {
  let g = { @called(atMostOnce) in f() }
  f = { }
  g()
  f()
}

func testWrappedInLocalClosureAndReassignedMultiConsume(_ f: @escaping @called(atMostOnce) () -> Void) { // expected-error {{'f' consumed more than once}}
  let g = { @called(atMostOnce) in f() } // expected-note {{consumed here}}
  f() // expected-note {{consumed again here}}
  f = { }
  g() // Ok
  f() // Ok
}

func passThroughParam(fn: @called(atMostOnce) () -> Void) {
  testCallAtMostOnce(fn) // Ok
}

func passThroughEscapingParam(fn: @escaping @called(atMostOnce) () -> Void) {
  testCallAtMostOnce(fn) // Ok
}

func testCallAtMostOnceThenForward(_ f: @called(atMostOnce) () -> Void) { // expected-error {{'f' consumed more than once}}
  f() // expected-note {{consumed here}}
  testCallAtMostOnce(f) // expected-note {{consumed again here}}
}

func passThroughClosureOptional(fn: consuming (@called(atMostOnce) () -> Void)?) {
  guard let fn else { return }
  fn()
}

func passThroughClosureOptionalAndUnwrap(fn: consuming (@called(atMostOnce) () -> Void)?) { // expected-error {{'fn' consumed more than once}}
  fn?() // expected-note {{consumed here}}
  guard let fn else { return } // expected-note {{consumed again here}}
  _ = fn
}

func testEarlyReturnBeforeCall(cond: Bool, _ f: @escaping @called(atMostOnce) () -> Void) {
  if cond { return }
  f()
}

func passConditionalThroughParamThenCall(cond: Bool, fn: @called(atMostOnce) () -> Void) { // expected-error {{'fn' consumed more than once}}
  if cond {
    testCallAtMostOnce(fn) // expected-note {{consumed here}}
  }
  fn() // expected-note {{consumed again here}}
}

func localCaptureThenCall() {
  let fn: @called(atMostOnce) () -> Void = { } // expected-error {{'fn' consumed more than once}}
  _ = { @called(atMostOnce) [fn] in testCallAtMostOnce(fn) } // expected-note {{consumed here}}
  fn() // expected-note {{consumed again here}}

  let other: @called(atMostOnce) () -> Void = { } // expected-error {{'other' consumed more than once}}
  _ = { @called(atMostOnce) in testCallAtMostOnce(other) } // expected-note {{consumed here}}
  other() // expected-note {{consumed again here}}
}

func captureInRegularClosure(
  f: @escaping @called(atMostOnce) () -> Void,
  g: @escaping @called(atMostOnce) () -> Void,
  optF: consuming (@called(atMostOnce) () -> Void)? = nil
) {
  _ = { [f] in f() } // expected-error {{noncopyable 'f' cannot be consumed when captured by an escaping closure or borrowed by a non-Escapable type}}
  _ = { g() } // expected-error {{noncopyable 'g' cannot be consumed when captured by an escaping closure or borrowed by a non-Escapable type}}

  guard let optF else { return }
  _ = { @called(atMostOnce) in optF() } // Ok
}

func testTwoIndependentParams(_ f: @called(atMostOnce) () -> Void, _ g: @called(atMostOnce) () -> Void) {
  f()
  g()
}

func testTwoIndependentParamsOneDoubled(_ f: @called(atMostOnce) () -> Void, _ g: @called(atMostOnce) () -> Void) { // expected-error {{'g' consumed more than once}}
  f()
  g() // expected-note {{consumed here}}
  g() // expected-note {{consumed again here}}
}

func testSwitchEachCaseCallsAtMostOnce(_ x: Int, _ f: @called(atMostOnce) () -> Void) {
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
func testParamCaptureThenCallDirectly(f: @escaping @called(atMostOnce) () -> Void) { // expected-error {{'f' consumed more than once}}
  let a = { @called(atMostOnce) in f() } // expected-note {{consumed here}}
  f() // expected-note {{consumed again here}}
  a()
}

// Same value captured by two different `@called(atMostOnce)` closures without an
// intervening reassignment - only the first capture may consume it.
func testCapturedByTwoClosures(_ f: @escaping @called(atMostOnce) () -> Void) { // expected-error {{'f' consumed more than once}}
  let a = { @called(atMostOnce) in f() } // expected-note {{consumed here}}
  let b = { @called(atMostOnce) in f() } // expected-note {{consumed again here}}
  a()
  b()
}

// ... but capturing into two closures along mutually exclusive paths is fine.
func testCapturedByTwoClosuresMutuallyExclusive(_ f: @escaping @called(atMostOnce) () -> Void, cond: Bool) {
  if cond {
    let a = { @called(atMostOnce) in f() }
    a()
  } else {
    let b = { @called(atMostOnce) in f() }
    b()
  }
}

// A closure capturing multiple independent `@called(atMostOnce)` values may call
// each of them once.
func testClosureCapturesTwoIndependentValues(f: @escaping @called(atMostOnce) () -> Void, g: @escaping @called(atMostOnce) () -> Void) {
  let a = { @called(atMostOnce) in
    f()
    g()
  }
  a()
}

// The "at most once" checking still applies *inside* the closure body to its
// own captures.
func testClosureCapturesAndDoubleCallsOneOfThem(f: @escaping @called(atMostOnce) () -> Void, g: @called(atMostOnce) () -> Void) { // expected-error 2 {{'f' consumed more than once}}
  let a = { @called(atMostOnce) in
    f() // expected-note 2 {{consumed here}}
    f() // expected-note 2 {{consumed again here}}
  }
  a()
}

// A `@called(atMostOnce)` value can also be captured alongside ordinary (copyable)
// values without disturbing their capture kind.
func testMixedCaptureKinds(_ f: @escaping @called(atMostOnce) () -> Void, x: Int) {
  let g = { @called(atMostOnce) in
    print(x)
    f()
  }
  g()
}

// Nesting: an outer `@called(atMostOnce)` closure capturing an inner one that
// itself captures the original value.
func testNestedCalledAtMostOnceClosures(_ f: @escaping @called(atMostOnce) () -> Void) {
  let inner = { @called(atMostOnce) in f() }
  let outer = { @called(atMostOnce) in inner() }
  outer()
}

func testNestedCalledAtMostOnceClosuresDouble(_ f: @escaping @called(atMostOnce) () -> Void) {
  let inner = { @called(atMostOnce) in f() }
  let outer = { @called(atMostOnce) in inner() }
  // expected-error@-1 {{'outer' consumed more than once}}
  outer() // expected-note {{consumed here}}
  outer() // expected-note {{consumed again here}}
}

// Returning a closure that captures a `@called(atMostOnce)` value: the capture is
// consumed where the closure literal is formed, not where it's later called.
func makeWrapper(_ f: @escaping @called(atMostOnce) () -> Void) -> @called(atMostOnce) () -> Void {
  return { @called(atMostOnce) in f() }
}

func testUseWrapper(_ f: @escaping @called(atMostOnce) () -> Void) {
  let w = makeWrapper(f)
  w()
}

// A captured `var` follows the same "consumed at formation" rule as a `let`;
// reassigning it afterward is ordinary reinitialization, and using the new
// value again later is fine.
func testVarCaptureReassignedAfterFormation(_ f: @escaping @called(atMostOnce) () -> Void) {
  var f = f
  let g = { @called(atMostOnce) in f() }
  f = makeClosure()
  g()
  f()
}

// Without the reassignment, using `f` again after it was captured is a
// double consumption, exactly as it would be for a `let`.
func testVarCaptureUsedAgainWithoutReassignment(_ f: @escaping @called(atMostOnce) () -> Void) {
  var f = f // expected-warning {{variable 'f' was never mutated; consider changing to 'let' constant}}
  // expected-error@-1 {{'f' consumed more than once}}
  let g = { @called(atMostOnce) in f() } // expected-note {{consumed here}}
  g()
  f() // expected-note {{consumed again here}}
}

// Explicit capture-list renaming (`[g2 = f]`) goes through the same
// consuming-capture path as a plain `[f]` capture.
func testCaptureListRename(_ f: @escaping @called(atMostOnce) () -> Void) {
  let g = { @called(atMostOnce) [g2 = f] in g2() }
  g()
}

// Use `@called(atMostOnce)` as a property type, the parameter is implicitly `consuming`.
do {
  struct S1: ~Copyable {
    let operation: @called(atMostOnce) () -> Void
    
    init(operation: @escaping @called(atMostOnce) () -> Void) {
      // okay; the parameter is implicitly 'consuming'
      self.operation = operation
    }
    
    consuming func call() {
      operation()
    }
  }

  struct S2: ~Copyable {
    let operation: @called(atMostOnce) () -> Void
    
    init(operation: @escaping @called(atMostOnce) () -> Void) {
      self.operation = operation
    }

    deinit { // expected-note {{deinitializer declared here}}
    }

    consuming func call() {
      operation() // expected-error {{cannot partially consume 'self' when it has a deinitializer}}
    }
  }
}
