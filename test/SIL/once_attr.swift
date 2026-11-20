// RUN: %target-swift-frontend %s \
// RUN: -emit-sil -target %target-swift-5.1-abi-triple \
// RUN: -enable-experimental-feature OnceFunctions \
// RUN: -verify

// REQUIRES: swift_feature_OnceFunctions

func makeClosure() -> @once () -> Void {
  return {}
}

func testCallOnce(_ f: @once () -> Void) {
  f()
}

func testCalledInBothBranches(cond: Bool, _ f: @once () -> Void) {
  if cond {
    f()
  } else {
    f()
  }
}

func testMoveThenCall(_ f: @once () -> Void) {
  let g = f
  g()
}

func testImmediateCallOnReturnValue() {
  makeClosure()() // Ok
}

func passThroughParam(fn: @once () -> Void) {
  testCallOnce(fn) // Ok
}

func testDoubleCall(_ f: @once () -> Void) { // expected-error {{'f' consumed more than once}}
  f() // expected-note {{consumed here}}
  f() // expected-note {{consumed again here}}
}

func testCalledInLoop(_ f: @once () -> Void) { // expected-error {{'f' consumed in a loop}}
  for _ in 0..<10 {
    f() // expected-note {{consumed here}}
  }
}

func testCallThenForward(_ f: @once () -> Void) { // expected-error {{'f' consumed more than once}}
  f() // expected-note {{consumed here}}
  testCallOnce(f) // expected-note {{consumed again here}}
}

// MARK: - Captures

// A closure that might run more than once can't consume what it captures, and
// any use of an exactly-once value consumes it.

func takesPlain(_: () -> Void) {}

// FIXME: [called-once] The move-only checker marks a function that it diagnosed, so that the implicit destroy checker
// skips it, but a closure is move-checked after the function that contains it. A mistake in a closure's captures is
// therefore also reported as the value not being called in the enclosing function. Each "is not called" error in this
// section is redundant.
func capturedByEscapingClosure(_ f: @escaping @once () -> Void) { // expected-error {{missing reinitialization of closure capture 'f' after consume}}
  // expected-error@-1 {{'f' is not called on all paths}}
  let g = { f() } // expected-note {{consumed here}}
  g()
} // expected-note {{path exits here without a call}}

func capturedByNonescapingClosure(_ f: @escaping @once () -> Void) { // expected-error {{missing reinitialization of closure capture 'f' after consume}}
  // expected-error@-1 {{'f' is not called on all paths}}
  takesPlain { f() } // expected-note {{consumed here}}
} // expected-note {{path exits here without a call}}

func capturedByLocalFunction(_ f: @escaping @once () -> Void) { // expected-error {{missing reinitialization of closure capture 'f' after consume}}
  // expected-error@-1 {{'f' is not called on all paths}}
  func local() {
    f() // expected-note {{consumed here}}
  }
  local()
} // expected-note {{path exits here without a call}}

func capturedThroughPlainClosure(_ f: @escaping @once () -> Void) { // expected-error {{missing reinitialization of closure capture 'f' after consume}}
  // expected-error@-1 {{'f' is not called on all paths}}
  let outer = {
    let inner = { @once in f() } // expected-note {{consumed here}}
    inner()
  }
  outer()
} // expected-note {{path exits here without a call}}

// FIXME: [called-once] A `defer` body runs exactly once, so it should be able
// to call a captured exactly-once value.
func capturedByDefer(_ f: @escaping @once () -> Void) { // expected-error {{missing reinitialization of closure capture 'f' after consume}}
  // expected-error@-1 {{'f' is not called on all paths}}
  defer { f() } // expected-note {{consumed here}}
  print("body")
} // expected-note {{path exits here without a call}}

// MARK: - Calls on all paths

struct SomeError: Error {}

enum Choice {
  case a, b, c
}

func neverCalled(_ f: @once () -> Void) { // expected-error {{'f' is not called on all paths}}
} // expected-note {{path exits here without a call}}

func calledOnOneBranch(_ cond: Bool, _ f: @once () -> Void) { // expected-error {{'f' is not called on all paths}}
  if cond { // expected-note {{path exits here without a call}}
    f()
  }
}

func switchMissingCase(_ choice: Choice, _ f: @once () -> Void) { // expected-error {{'f' is not called on all paths}}
  switch choice {
  case .a:
    f()
  case .b:
    f()
  case .c:
    break // expected-note {{path exits here without a call}}
  }
}

func switchDefaultWithoutCall(_ choice: Choice, _ f: @once () -> Void) { // expected-error {{'f' is not called on all paths}}
  switch choice {
  case .a:
    f()
  default:
    print("no call") // expected-note {{path exits here without a call}}
  }
}

func switchFallthrough(_ choice: Choice, _ f: @once () -> Void) { // expected-error {{'f' is not called on all paths}}
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

func consumeWithoutCall(_ f: @once () -> Void) { // expected-error {{'f' is not called on all paths}}
  _ = consume f // expected-note {{path exits here without a call}}
}

func moveWithoutCall(_ f: @once () -> Void) {
  let g = consume f // expected-error {{'g' is not called on all paths}}
  // expected-warning@-1 {{initialization of immutable value 'g' was never used}}
} // expected-note {{path exits here without a call}}

func guardReturnsWithoutCall(_ cond: Bool, _ f: @once () -> Void) { // expected-error {{'f' is not called on all paths}}
  guard cond else {
    return // expected-note {{path exits here without a call}}
  }
  f()
}

func throwsWithoutCall(_ cond: Bool, _ f: @once () -> Void) throws { // expected-error {{'f' is not called on all paths}}
  if cond {
    throw SomeError() // expected-note {{path exits here without a call}}
  }
  f()
}

func discardsResult() {
  _ = makeClosure() // expected-error {{'@once' function is not called on all paths}}
  // expected-note@-1 {{path exits here without a call}}
}

func overwritesVar(_ f: @once () -> Void) {
  var g = f // expected-error {{'g' is not called on all paths}}
  g = makeClosure() // expected-note {{path exits here without a call}}
  g()
}

func overwrite(_ f: inout @once () -> Void) { // expected-error {{'f' is not called on all paths}}
  f = makeClosure() // expected-note {{path exits here without a call}}
}

func closureCallsOnOneBranch(_ cond: Bool, _ f: @escaping @once () -> Void) -> @once () -> Void {
  return { @once in // expected-error {{'f' is not called on all paths}}
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
func capturedInBoxByEscapingClosure(_ f: @escaping @once () -> Void) {
  var g = f // expected-error {{'g' is not called on all paths}}
  escapedClosure = { callAndReplace(&g) }
} // expected-note {{path exits here without a call}}


// MARK: - Accepted

func calledInEverySwitchCase(_ choice: Choice, _ f: @once () -> Void) {
  switch choice {
  case .a:
    f()
  case .b, .c:
    f()
  }
}

func endsInFatalError(_ cond: Bool, _ f: @once () -> Void) {
  if cond {
    fatalError()
  }
  f()
}

func neverReturns() -> Never {
  fatalError()
}

func endsInNeverCall(_ f: @once () -> Void) {
  neverReturns()
}

func transfersByReturn(_ f: @escaping @once () -> Void) -> @once () -> Void {
  return f
}

func transfersByCapture(_ f: @escaping @once () -> Void) -> @once () -> Void {
  return { @once in f() }
}

func transfersToInout(_ f: inout @once () -> Void) {
  f()
  f = makeClosure()
}

func callsInInfiniteLoop() -> Never {
  while true {
    let g = makeClosure()
    g()
  }
}

func callAndReplace(_ f: inout @once () -> Void) {
  f()
  f = makeClosure()
}

func capturedInoutByNonescapingClosure(_ f: @escaping @once () -> Void) {
  var g = f
  takesPlain { callAndReplace(&g) }
  g()
}

// MARK: - rethrows

func rethrowsCallOnce<R>(_ body: @once () throws -> R) rethrows -> R {
  try body()
}

func rethrowsCallConsumedOnce<R>(_ body: @once () throws -> R) rethrows -> R {
  try (consume body)()
}

func rethrowsForwardOnce<R>(_ body: @once () throws -> R) rethrows -> R {
  try rethrowsCallOnce(body)
}

func rethrowsForwardConsumedOnce<R>(
  _ body: @once () throws -> R
) rethrows -> R {
  try rethrowsCallOnce(consume body)
}

func rethrowsCallCopiedConsuming(
  _ body: consuming @escaping () throws -> Void
) rethrows {
  try (copy body)()
}

func callRethrowsOnce() throws -> Int {
  let a = rethrowsCallOnce { 1 }
  let b = rethrowsCallConsumedOnce { 2 }
  let c = rethrowsForwardOnce { 3 }
  let d = rethrowsForwardConsumedOnce { 4 }
  rethrowsCallCopiedConsuming {}
  let e = try rethrowsForwardOnce { () throws -> Int in throw SomeError() }
  return a + b + c + d + e
}

// MARK: - Borrowed values

// A borrowed exactly-once value can't be called, because calling it consumes
// it. It can be inspected and borrowed again.

func inspect(_ f: borrowing @once () -> Void) {}
func inspectEscaping(_ f: borrowing @escaping @once () -> Void) {}
func run(_ body: () -> Void) { body() }

func borrowedForwarded(_ f: borrowing @once () -> Void) {
  inspect(f)
  inspect(f)
}

func borrowedEscapingForwarded(_ f: borrowing @escaping @once () -> Void) {
  inspectEscaping(f)
}

func borrowedThenCalled(_ f: @once () -> Void) {
  inspect(f)
  f()
}

func localBorrowedThenCalled(_ x: Int) {
  let f: @once () -> Void = { print(x) }
  inspectEscaping(f)
  f()
}

func borrowedByNonescapingClosure(_ f: borrowing @once () -> Void) {
  run { inspect(f) }
}

func borrowedQueries(
  _ f: borrowing @escaping @isolated(any) @once () -> Void
) -> (any Actor)? {
  _ = type(of: f)
  return f.isolation
}

func borrowedCalled(_ f: borrowing @once () -> Void) {
  // expected-error@-1 {{'f' is borrowed and cannot be called}}
  f() // expected-note {{called here; calling a '@once' function consumes it}}
}

func borrowedEscapingCalled(_ f: borrowing @escaping @once () -> Void) {
  // expected-error@-1 {{'f' is borrowed and cannot be called}}
  f() // expected-note {{called here; calling a '@once' function consumes it}}
}

func borrowedAsyncCalled(_ f: borrowing @once () async -> Void) async {
  // expected-error@-1 {{'f' is borrowed and cannot be called}}
  await f() // expected-note {{called here; calling a '@once' function consumes it}}
}

func borrowedCalledInBothBranches(_ b: Bool, _ f: borrowing @once () -> Void) {
  // expected-error@-1 {{'f' is borrowed and cannot be called}}
  if b {
    f() // expected-note {{called here; calling a '@once' function consumes it}}
  } else {
    f() // expected-note {{called here; calling a '@once' function consumes it}}
  }
}

func borrowedClosureParameterCalled() {
  let _: (borrowing @once () -> Void) -> Void = { f in
    // expected-error@-1 {{'f' is borrowed and cannot be called}}
    f() // expected-note {{called here; calling a '@once' function consumes it}}
  }
}

func legacySharedCalled(_ f: __shared @once () -> Void) {
  // expected-error@-1 {{'f' is borrowed and cannot be called}}
  f() // expected-note {{called here; calling a '@once' function consumes it}}
}

func borrowedPassedToCall(
  _ f: borrowing @once () -> Void,
  _ g: @once (consuming @once () -> Void) -> Void
) {
  // expected-error@-3 {{'f' is borrowed and cannot be consumed}}
  g(f) // expected-note {{consumed here}}
}

func borrowedBound(_ f: borrowing @once () -> Void) {
  // expected-error@-1 {{'f' is borrowed and cannot be consumed}}
  let g = f // expected-note {{consumed here}}
  g()
}

func borrowedPassedConsuming(_ f: borrowing @once () -> Void) {
  // expected-error@-1 {{'f' is borrowed and cannot be consumed}}
  func take(_ f: @once () -> Void) { f() }
  take(f) // expected-note {{consumed here}}
}

// FIXME: Converting an escaping value to a nonescaping one consumes it, so a
// borrowed escaping value or a local can't be passed to a nonescaping
// borrowing parameter yet.
func borrowedEscapingToNonescaping(_ f: borrowing @escaping @once () -> Void) {
  // expected-error@-1 {{'f' is borrowed and cannot be consumed}}
  inspect(f) // expected-note {{consumed here}}
}

func localToNonescaping(_ x: Int) {
  let f: @once () -> Void = { print(x) } // expected-error {{'f' consumed more than once}}
  inspect(f) // expected-note {{consumed here}}
  f() // expected-note {{consumed again here}}
}

// FIXME: A closure that a coroutine uses isn't promoted to the stack, so it
// can't borrow a borrowed value yet.
struct ReadsWithBody {
  subscript(body: () -> Void) -> Int {
    _read {
      body()
      yield 0
    }
  }
}

func borrowedByCoroutineClosure(
  _ r: ReadsWithBody, _ f: borrowing @once () -> Void
) -> Int {
  // expected-error@-2 {{'f' cannot be captured by an escaping closure since it is a borrowed parameter}}
  return r[{ inspect(f) }] // expected-note {{closure capturing 'f' here}}
}

// Borrowing a value doesn't call it.
func localDroppedAfterBorrow(_ x: Int) {
  let f: @once () -> Void = { print(x) } // expected-error {{'f' is not called on all paths}}
  inspectEscaping(f)
} // expected-note {{path exits here without a call}}

func temporaryDroppedAfterBorrow(_ x: Int) {
  inspect({ print(x) }) // expected-error {{'@once' function is not called on all paths}}
  // expected-note@-1 {{path exits here without a call}}
}
