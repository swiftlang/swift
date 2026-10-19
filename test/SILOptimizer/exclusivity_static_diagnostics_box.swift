// RUN: %target-swift-frontend -emit-sil -verify -Onone -target %target-swift-6.2-abi-triple %s -o /dev/null
// RUN: %target-swift-frontend -emit-sil -verify -O -target %target-swift-6.2-abi-triple %s -o /dev/null

// Static diagnostics for variables captured by box: by a closure that may
// escape, or by a local function. Their accesses are dynamically enforced
// unless MandatoryAllocBoxToStack promotes the box, which it only does if a
// conflict either can't happen or happens on every path through the call. So
// a conflict is only diagnosed statically if dynamic enforcement would fail
// every time the call executes. The diagnostics must be the same at every
// optimization level. rdar://179406916

func use(_: Span<Int>) {}
func use(_: Int) {}

func callNow(_ body: @escaping () -> ()) {
  body()
}

func modifyAndCall(_ x: inout Int, _ body: @escaping () -> ()) {
  body()
}

// The callee calls the closure on every path.

func escapingClosureModifiesBorrowedArray() {
  var array = [1, 2, 3]
  let span = array.span // expected-note {{conflicting access is here}}
  callNow {
    array = [] // expected-error {{overlapping accesses to 'array', but modification requires exclusive access; consider copying to a local variable}}
  }
  use(span)
}

func escapingClosureModifiesInoutArgument() {
  var count = 0
  modifyAndCall(&count) { // expected-error {{overlapping accesses to 'count', but modification requires exclusive access; consider copying to a local variable}}
    count += 1 // expected-note {{conflicting access is here}}
  }
}

func callForwarded(_ body: @escaping () -> ()) {
  callNow(body)
}

func escapingClosureForwarded() {
  var array = [1, 2, 3]
  let span = array.span // expected-note {{conflicting access is here}}
  callForwarded {
    array = [] // expected-error {{overlapping accesses to 'array', but modification requires exclusive access; consider copying to a local variable}}
  }
  use(span)
}

// Reads don't conflict with reads.

func escapingClosureReadsBorrowedArray() {
  var array = [1, 2, 3]
  array.append(4)
  let span = array.span
  callNow {
    use(array.count)
  }
  use(span)
}

// The callee may not call the closure. Dynamic enforcement accepts the
// program if it doesn't.

func callIf(_ flag: Bool, _ body: @escaping () -> ()) {
  if flag {
    body()
  }
}

func callInLoop(_ n: Int, _ body: @escaping () -> ()) {
  for _ in 0..<n {
    body()
  }
}

func callUnlessTrapping(_ flag: Bool, _ body: @escaping () -> ()) {
  if flag {
    fatalError()
  }
  body()
}

func ignore(_ body: @escaping () -> ()) {}

var savedClosure: (() -> ())?

func save(_ body: @escaping () -> ()) {
  savedClosure = body
}

func escapingClosureMayBeCalled(_ flag: Bool, _ n: Int) {
  var array = [1, 2, 3]
  let span = array.span
  callIf(flag) {
    array = []
  }
  callInLoop(n) {
    array = []
  }
  callUnlessTrapping(flag) {
    array = []
  }
  ignore {
    array = []
  }
  save {
    array = []
  }
  use(span)
}

// The callee never calls the closure. Dynamic enforcement accepts the program.

func escapingClosureNeverCalled() {
  var array = [1, 2, 3]
  let span = array.span
  ignore {
    array = []
  }
  use(span)
}

// A local function forms the escaping closure. MandatoryAllocBoxToStack promotes the variable's box to an argument of the
// local function, so the closure is still a specialization that DiagnoseStaticExclusivity follows.

func localFunctionFormsEscapingClosure() {
  var array = [1, 2, 3]
  let span = array.span // expected-note {{conflicting access is here}}
  func modify() {
    callNow {
      array = [] // expected-error {{overlapping accesses to 'array', but modification requires exclusive access; consider copying to a local variable}}
    }
  }
  modify()
  use(span)
}

// The closure may not access the variable.

func escapingClosureMayModify(_ flag: Bool) {
  var array = [1, 2, 3]
  let span = array.span
  callNow {
    if flag {
      array = []
    }
  }
  use(span)
}

// A local function that captures the variable by box.

func localFunctionModifies() {
  var array = [1, 2, 3]
  func modify() {
    array = [] // expected-error {{overlapping accesses to 'array', but modification requires exclusive access; consider copying to a local variable}}
  }
  let span = array.span // expected-note {{conflicting access is here}}
  modify()
  use(span)
}

func localFunctionMayModify(_ flag: Bool) {
  var array = [1, 2, 3]
  func modify() {
    if flag {
      array = []
    }
  }
  let span = array.span
  modify()
  use(span)
}

// A non-escaping argument that is a closure capturing the variable by box.

func readAndPerform(_ x: UnsafePointer<Int>, closure: () -> ()) {
  closure()
}

func modifyAndPerform(_ x: UnsafeMutablePointer<Int>, closure: () -> ()) {
  closure()
}

// The closure is converted to a non-escaping closure, which
// MandatoryAllocBoxToStack doesn't promote, so the conflict is caught at run
// time (Interpreter/exclusivity_captured_box.swift).
func noescapeArgumentCapturesBox() {
  var x = 3
  let c = { x = 42 }
  readAndPerform(&x, closure: c)
  _ = x
}

func noescapeArgumentMayAccessBox(_ flag: Bool) {
  var x = 3
  let c = { if flag { x = 42 } }
  modifyAndPerform(&x, closure: c)
  _ = x
}

// rdar://102056143: a violation through recursive captures. local1 only
// modifies flag (through local2) on some paths, so flag stays a box and the
// conflict traps at run time (Interpreter/enforce_exclusive_access.swift).
func recursiveCaptureViolation() -> Bool {
  var flag = false
  func local1(_ x: inout Bool) {
    if !flag {
      x = true
      local2()
    }
  }
  func local2() {
    local1(&flag)
  }
  local2()
  return flag
}

// Control flow in a local function that modifies the variable while a Span
// borrows it. A conflict is only diagnosed if the local function modifies the
// variable on every path that returns or throws. A path that traps doesn't
// count.

func neverReturns() -> Never { fatalError() }

func localFunctionModifiesOnBothArms(_ flag: Bool) {
  var array = [1, 2, 3]
  func modify() {
    if flag { array = [] } else { array = [4] } // expected-error {{overlapping accesses to 'array', but modification requires exclusive access; consider copying to a local variable}}
  }
  let span = array.span // expected-note {{conflicting access is here}}
  modify()
  use(span)
}

func localFunctionModifiesElseTraps(_ flag: Bool) {
  var array = [1, 2, 3]
  func modify() {
    if flag { array = [] } else { fatalError() } // expected-error {{overlapping accesses to 'array', but modification requires exclusive access; consider copying to a local variable}}
  }
  let span = array.span // expected-note {{conflicting access is here}}
  modify()
  use(span)
}

func localFunctionModifiesElseNeverReturns(_ flag: Bool) {
  var array = [1, 2, 3]
  func modify() {
    if flag { array = [] } else { neverReturns() } // expected-error {{overlapping accesses to 'array', but modification requires exclusive access; consider copying to a local variable}}
  }
  let span = array.span // expected-note {{conflicting access is here}}
  modify()
  use(span)
}

func localFunctionModifiesElseLoopsThenTraps(_ flag: Bool, _ n: Int) {
  var array = [1, 2, 3]
  func modify() {
    if flag {
      array = [] // expected-error {{overlapping accesses to 'array', but modification requires exclusive access; consider copying to a local variable}}
    } else {
      for _ in 0..<n {}
      fatalError()
    }
  }
  let span = array.span // expected-note {{conflicting access is here}}
  modify()
  use(span)
}

// A local function that only reads the variable doesn't conflict with a read.

func localFunctionReadsOnly() {
  var array = [1, 2, 3]
  array.append(4)
  func read() {
    use(array.count)
  }
  let span = array.span
  read()
  use(span)
}

// A read on every path doesn't decide a conflict with a read in progress: the
// local function only modifies on some paths.

func localFunctionReadsAlwaysModifiesSometimes(_ flag: Bool) {
  var array = [1, 2, 3]
  func update() {
    let n = array.count
    if flag {
      array = [n]
    }
  }
  let span = array.span
  update()
  use(span)
}

// The local function modifies the variable on every path, but also calls a
// local function that only modifies it on some paths, possibly while its own
// access is in progress. Dynamic enforcement checks the conflict.

func localFunctionForwardsToMayModify(_ flag: Bool) {
  var array = [1, 2, 3]
  func mayModify() {
    if flag {
      array = []
    }
  }
  func modify() {
    array = [4]
    mayModify()
  }
  let span = array.span
  modify()
  use(span)
}

// The local function modifies the variable after a call that may throw, so it
// doesn't modify it on the path that throws.

struct Failure: Error {}

func mayThrow(_ flag: Bool) throws {
  if flag {
    throw Failure()
  }
}

func localFunctionThrowsBeforeModify(_ flag: Bool) throws {
  var array = [1, 2, 3]
  func modify() throws {
    try mayThrow(flag)
    array = []
  }
  let span = array.span
  try modify()
  use(span)
}

// Both a local function and an escaping closure capture the variable and are
// used by the same call. Static enforcement can't check accesses through
// two arguments of a call against each other, so the variable isn't promoted
// and the conflict is only caught at run time
// (Interpreter/exclusivity_captured_box.swift).

func modifyAndCallClosure(_ x: inout Int, _ body: () -> ()) {
  body()
}

func localFunctionAndClosureCaptureVariable() {
  var x = 0
  func outer(_ c: @escaping () -> ()) {
    modifyAndCallClosure(&x) { c() }
  }
  outer { x += 1 }
}

// The variable is captured twice by arguments of the same call, but the
// accesses don't overlap.

func callBoth(_ a: () -> (), _ b: () -> ()) {
  a()
  b()
}

func localFunctionPassedTwice() {
  var x = 0
  func increment() {
    x += 1
  }
  callBoth(increment, increment)
}

// Local functions that only access their captured variables, called directly
// (rdar://157458037). No conflict, and the boxes are promoted.

func localFunctionsWithoutConflict() -> Int {
  var a1 = 1
  var a2 = 2
  func innerFunction() {
    a1 += 1
    a2 += 2
  }
  innerFunction()
  innerFunction()
  return a1 + a2
}
