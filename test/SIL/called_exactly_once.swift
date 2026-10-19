// RUN: %target-swift-frontend %s \
// RUN: -emit-sil -target %target-swift-5.1-abi-triple \
// RUN: -enable-experimental-feature CalledAttribute \
// RUN: -verify

// REQUIRES: swift_feature_CalledAttribute

func makeClosure() -> @called(exactlyOnce) () -> Void {
  return {}
}

func testCallOnce(_ f: @called(exactlyOnce) () -> Void) {
  f()
}

func testCalledInBothBranches(cond: Bool, _ f: @called(exactlyOnce) () -> Void) {
  if cond {
    f()
  } else {
    f()
  }
}

func testMoveThenCall(_ f: @called(exactlyOnce) () -> Void) {
  let g = f
  g()
}

func testImmediateCallOnReturnValue() {
  makeClosure()() // Ok
}

func passThroughParam(fn: @called(exactlyOnce) () -> Void) {
  testCallOnce(fn) // Ok
}

func testDoubleCall(_ f: @called(exactlyOnce) () -> Void) { // expected-error {{'f' consumed more than once}}
  f() // expected-note {{consumed here}}
  f() // expected-note {{consumed again here}}
}

func testCalledInLoop(_ f: @called(exactlyOnce) () -> Void) { // expected-error {{'f' consumed in a loop}}
  for _ in 0..<10 {
    f() // expected-note {{consumed here}}
  }
}

func testCallThenForward(_ f: @called(exactlyOnce) () -> Void) { // expected-error {{'f' consumed more than once}}
  f() // expected-note {{consumed here}}
  testCallOnce(f) // expected-note {{consumed again here}}
}

// MARK: - Captures

// A closure that might run more than once can't consume what it captures, and
// any use of a `@called(exactlyOnce)` value consumes it.

func takesPlain(_: () -> Void) {}

// FIXME: [called-once] The move-only checker marks a function that it diagnosed, so that the implicit destroy checker
// skips it, but a closure is move-checked after the function that contains it. A mistake in a closure's captures is
// therefore also reported as the value not being called in the enclosing function. Each "is not called" error in this
// section is redundant.
func capturedByEscapingClosure(_ f: @escaping @called(exactlyOnce) () -> Void) { // expected-error {{missing reinitialization of closure capture 'f' after consume}}
  // expected-error@-1 {{'f' is not called on all paths}}
  let g = { f() } // expected-note {{consumed here}}
  g()
} // expected-note {{path exits here without a call}}

func capturedByNonescapingClosure(_ f: @escaping @called(exactlyOnce) () -> Void) { // expected-error {{missing reinitialization of closure capture 'f' after consume}}
  // expected-error@-1 {{'f' is not called on all paths}}
  takesPlain { f() } // expected-note {{consumed here}}
} // expected-note {{path exits here without a call}}

func capturedByLocalFunction(_ f: @escaping @called(exactlyOnce) () -> Void) { // expected-error {{'f' is not called on all paths}}
  func local() {
    f() // expected-error {{non-'Copyable' 'f' cannot be consumed when captured by an escaping closure or borrowed by a non-'Escapable' type}}
  }
  local()
} // expected-note {{path exits here without a call}}

func capturedThroughPlainClosure(_ f: @escaping @called(exactlyOnce) () -> Void) { // expected-error {{missing reinitialization of closure capture 'f' after consume}}
  // expected-error@-1 {{'f' is not called on all paths}}
  let outer = {
    let inner = { @called(exactlyOnce) in f() } // expected-note {{consumed here}}
    inner()
  }
  outer()
} // expected-note {{path exits here without a call}}

// FIXME: [called-once] A `defer` body runs exactly once, so it should be able
// to call a captured `@called(exactlyOnce)` value.
func capturedByDefer(_ f: @escaping @called(exactlyOnce) () -> Void) { // expected-error {{missing reinitialization of closure capture 'f' after consume}}
  // expected-error@-1 {{'f' is not called on all paths}}
  defer { f() } // expected-note {{consumed here}}
  print("body")
} // expected-note {{path exits here without a call}}

// MARK: - Calls on all paths

struct SomeError: Error {}

enum Choice {
  case a, b, c
}

func neverCalled(_ f: @called(exactlyOnce) () -> Void) { // expected-error {{'f' is not called on all paths}}
} // expected-note {{path exits here without a call}}

func calledOnOneBranch(_ cond: Bool, _ f: @called(exactlyOnce) () -> Void) { // expected-error {{'f' is not called on all paths}}
  if cond { // expected-note {{path exits here without a call}}
    f()
  }
}

func switchMissingCase(_ choice: Choice, _ f: @called(exactlyOnce) () -> Void) { // expected-error {{'f' is not called on all paths}}
  switch choice {
  case .a:
    f()
  case .b:
    f()
  case .c:
    break // expected-note {{path exits here without a call}}
  }
}

func switchDefaultWithoutCall(_ choice: Choice, _ f: @called(exactlyOnce) () -> Void) { // expected-error {{'f' is not called on all paths}}
  switch choice {
  case .a:
    f()
  default:
    print("no call") // expected-note {{path exits here without a call}}
  }
}

func switchFallthrough(_ choice: Choice, _ f: @called(exactlyOnce) () -> Void) { // expected-error {{'f' is not called on all paths}}
  switch choice {
  case .a:
    fallthrough
  case .b:
    f()
  case .c:
    fallthrough
  default:
    print("no call") // expected-note {{path exits here without a call}}
  }
}

func consumeWithoutCall(_ f: @called(exactlyOnce) () -> Void) { // expected-error {{'f' is not called on all paths}}
  _ = consume f // expected-note {{path exits here without a call}}
}

func moveWithoutCall(_ f: @called(exactlyOnce) () -> Void) {
  let g = consume f // expected-error {{'g' is not called on all paths}}
  // expected-warning@-1 {{initialization of immutable value 'g' was never used}}
} // expected-note {{path exits here without a call}}

func guardReturnsWithoutCall(_ cond: Bool, _ f: @called(exactlyOnce) () -> Void) { // expected-error {{'f' is not called on all paths}}
  guard cond else {
    return // expected-note {{path exits here without a call}}
  }
  f()
}

func throwsWithoutCall(_ cond: Bool, _ f: @called(exactlyOnce) () -> Void) throws { // expected-error {{'f' is not called on all paths}}
  if cond {
    throw SomeError() // expected-note {{path exits here without a call}}
  }
  f()
}

func discardsResult() {
  _ = makeClosure() // expected-error {{'@called(exactlyOnce)' function is not called on all paths}}
  // expected-note@-1 {{path exits here without a call}}
}

func overwritesVar(_ f: @called(exactlyOnce) () -> Void) {
  var g = f // expected-error {{'g' is not called on all paths}}
  g = makeClosure() // expected-note {{path exits here without a call}}
  g()
}

func overwrite(_ f: inout @called(exactlyOnce) () -> Void) { // expected-error {{'f' is not called on all paths}}
  f = makeClosure() // expected-note {{path exits here without a call}}
}

func closureCallsOnOneBranch(_ cond: Bool, _ f: @escaping @called(exactlyOnce) () -> Void) -> @called(exactlyOnce) () -> Void {
  return { @called(exactlyOnce) in // expected-error {{'f' is not called on all paths}}
    if cond { // expected-note {{path exits here without a call}}
      f()
    }
  }
}

func condition() -> Bool { true }

func dropsInInfiniteLoop() -> Never {
  while true {
    let g = makeClosure() // expected-error {{'g' is not called on all paths}}
    _ = g
  }
}

func dropsInLoopThatMayTrap() -> Never {
  while true {
    let g = makeClosure() // expected-error {{'g' is not called on all paths}}
    if condition() { fatalError() }
    _ = g
  }
}

var escapedClosure: (() -> Void)? = nil

// The escaping closure keeps `g` in its box, and destroying the box destroys
// `g` without calling it.
func capturedInBoxByEscapingClosure(_ f: @escaping @called(exactlyOnce) () -> Void) {
  var g = f // expected-error {{'g' is not called on all paths}}
  escapedClosure = { callAndReplace(&g) }
} // expected-note {{path exits here without a call}}


// MARK: - Accepted

func calledInEverySwitchCase(_ choice: Choice, _ f: @called(exactlyOnce) () -> Void) {
  switch choice {
  case .a:
    f()
  case .b, .c:
    f()
  }
}

func endsInFatalError(_ cond: Bool, _ f: @called(exactlyOnce) () -> Void) {
  if cond {
    fatalError()
  }
  f()
}

func neverReturns() -> Never {
  fatalError()
}

func endsInNeverCall(_ f: @called(exactlyOnce) () -> Void) {
  neverReturns()
}

func transfersByReturn(_ f: @escaping @called(exactlyOnce) () -> Void) -> @called(exactlyOnce) () -> Void {
  return f
}

func transfersByCapture(_ f: @escaping @called(exactlyOnce) () -> Void) -> @called(exactlyOnce) () -> Void {
  return { @called(exactlyOnce) in f() }
}

func transfersToInout(_ f: inout @called(exactlyOnce) () -> Void) {
  f()
  f = makeClosure()
}

func callsInInfiniteLoop() -> Never {
  while true {
    let g = makeClosure()
    g()
  }
}

func callAndReplace(_ f: inout @called(exactlyOnce) () -> Void) {
  f()
  f = makeClosure()
}

func capturedInoutByNonescapingClosure(_ f: @escaping @called(exactlyOnce) () -> Void) {
  var g = f
  takesPlain { callAndReplace(&g) }
  g()
}

