// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out_Onone -enforce-exclusivity=checked -Onone
// RUN: %target-build-swift %s -o %t/a.out_O -enforce-exclusivity=checked -O
// RUN: %target-codesign %t/a.out_Onone
// RUN: %target-codesign %t/a.out_O
// RUN: %target-run %t/a.out_Onone
// RUN: %target-run %t/a.out_O
// REQUIRES: executable_test

// Conflicts on variables captured by box (by local functions and closures) that
// static checking can't decide, so they must trap at run time at every
// optimization level. Either the conflict only happens on some paths, or static
// checking can't see it: the variable reaches a call through two captures, or
// through a closure that escapes. MandatoryAllocBoxToStack doesn't promote these
// boxes, so their accesses stay dynamic. The compile-time side is in
// SILOptimizer/exclusivity_static_diagnostics_box.swift. rdar://179406916
//
// Functions are @inline(never): after inlining, LegacyDeadObjectElimination can
// delete a dead variable together with its conflicting dynamic accesses.

import StdlibUnittest

var tests = TestSuite("ExclusivityCapturedBox")

let yes = CommandLine.arguments.count > 0

// -----------------------------------------------------------------------------
// Conflicts on some paths. Each function is run once without the conflict and
// once with it.

// The callee calls the escaping closure only on some paths.
@inline(never)
func modifyAndCallIf(_ x: inout Int, _ flag: Bool, _ body: @escaping () -> ()) {
  if flag {
    body()
  }
}

@inline(never)
func escapingClosureMayBeCalledDuringAccess(_ flag: Bool) {
  var count = 0
  modifyAndCallIf(&count, flag) {
    count += 1
  }
  _blackHole(count)
}

tests.test("EscapingClosureNotCalledDuringAccess") {
  escapingClosureMayBeCalledDuringAccess(!yes)
}

tests.test("EscapingClosureCalledDuringAccess") {
  expectCrashLater(withMessage: "Simultaneous accesses")
  escapingClosureMayBeCalledDuringAccess(yes)
}

// A local function modifies the variable only on some paths.
@inline(never)
func localFunctionMayModifyDuringAccess(_ flag: Bool) {
  var x = 0
  @inline(never)
  func inner() {
    if flag {
      x += 1
    }
  }
  @inline(never)
  func outer(_ v: inout Int) {
    inner()
  }
  outer(&x)
  _blackHole(x)
}

tests.test("LocalFunctionDoesNotModifyDuringAccess") {
  localFunctionMayModifyDuringAccess(!yes)
}

tests.test("LocalFunctionModifiesDuringAccess") {
  expectCrashLater(withMessage: "Simultaneous accesses")
  localFunctionMayModifyDuringAccess(yes)
}

// A local function reads on every path and modifies on only some, while a read
// access is in progress.
@inline(never)
func readAndPerform(_ p: UnsafePointer<Int>, _ body: () -> ()) {
  body()
}

@inline(never)
func localFunctionReadsAlwaysModifiesSometimesDuringAccess(_ flag: Bool) {
  var x = 0
  @inline(never)
  func update() {
    let n = x
    if flag {
      x = n + 1
    }
  }
  readAndPerform(&x) {
    update()
  }
  _blackHole(x)
}

tests.test("LocalFunctionDoesNotModifyAfterReadDuringAccess") {
  localFunctionReadsAlwaysModifiesSometimesDuringAccess(!yes)
}

tests.test("LocalFunctionModifiesAfterReadDuringAccess") {
  expectCrashLater(withMessage: "Simultaneous accesses")
  localFunctionReadsAlwaysModifiesSometimesDuringAccess(yes)
}

// A local function modifies after a call that may throw.
struct Failure: Error {}

@inline(never)
func mayThrow(_ flag: Bool) throws {
  if flag {
    throw Failure()
  }
}

@inline(never)
func localFunctionThrowsBeforeModifyDuringAccess(_ flag: Bool) throws {
  var x = 0
  @inline(never)
  func modify() throws {
    try mayThrow(flag)
    x += 1
  }
  @inline(never)
  func outer(_ v: inout Int) throws {
    try modify()
  }
  try outer(&x)
  _blackHole(x)
}

tests.test("LocalFunctionThrowsBeforeModifyDuringAccess") {
  var didThrow = false
  do {
    try localFunctionThrowsBeforeModifyDuringAccess(yes)
  } catch {
    didThrow = true
  }
  expectTrue(didThrow)
}

tests.test("LocalFunctionModifiesAfterNoThrowDuringAccess") {
  expectCrashLater(withMessage: "Simultaneous accesses")
  try! localFunctionThrowsBeforeModifyDuringAccess(!yes)
}

// -----------------------------------------------------------------------------
// Conflicts on every path that static checking can't see.

// A local function modifies the variable on every path, and also calls a local
// function that only modifies it on some paths, so the box isn't promoted.
// `modify` conflicts whether or not `mayModify` does.
@inline(never)
func localFunctionForwardsToMayModifyDuringAccess(_ flag: Bool) {
  var x = 0
  @inline(never)
  func mayModify() {
    if flag {
      x += 1
    }
  }
  @inline(never)
  func modify() {
    x += 1
    mayModify()
  }
  @inline(never)
  func outer(_ v: inout Int) {
    modify()
  }
  outer(&x)
  _blackHole(x)
}

tests.test("LocalFunctionForwardsToMayModifyModifiesDuringAccess") {
  expectCrashLater(withMessage: "Simultaneous accesses")
  localFunctionForwardsToMayModifyDuringAccess(yes)
}

tests.test("LocalFunctionForwardsToMayModifyDoesNotModifyDuringAccess") {
  expectCrashLater(withMessage: "Simultaneous accesses")
  localFunctionForwardsToMayModifyDuringAccess(!yes)
}

// An escaped closure is called during an access.
var saved: (() -> ())?

@inline(never)
func callSaved(_ x: inout Int) {
  saved!()
}

@inline(never)
func escapedClosureCalledDuringAccess() {
  var count = 0
  saved = { count += 1 }
  callSaved(&count)
  _blackHole(count)
}

tests.test("EscapedClosureCalledDuringAccess") {
  expectCrashLater(withMessage: "Simultaneous accesses")
  escapedClosureCalledDuringAccess()
}

// An escaping closure that captures the variable by box is converted to a
// non-escaping argument, passed along with an access to the variable.
@inline(never)
func readAndCall(_ x: UnsafePointer<Int>, closure: () -> ()) {
  closure()
}

@inline(never)
func noescapeArgumentCapturesBox() {
  var x = 3
  let c = { x = 42 }
  readAndCall(&x, closure: c)
  _blackHole(x)
}

tests.test("NoescapeArgumentCapturesBox") {
  expectCrashLater(withMessage: "Simultaneous accesses")
  noescapeArgumentCapturesBox()
}

// A local function and an escaping closure that both capture the variable are
// used by the same call. Static checking can't check accesses through two
// arguments of a call against each other.
@inline(never)
func modifyAndCallClosure(_ x: inout Int, _ body: () -> ()) {
  body()
}

@inline(never)
func localFunctionAndClosureCaptureVariable() {
  var x = 0
  @inline(never)
  func outer(_ c: @escaping () -> ()) {
    modifyAndCallClosure(&x) { c() }
  }
  outer { x += 1 }
  _blackHole(x)
}

tests.test("LocalFunctionAndClosureCaptureVariable") {
  expectCrashLater(withMessage: "Simultaneous accesses")
  localFunctionAndClosureCaptureVariable()
}

// The same with a non-escaping closure, which captures the variable by address
// and is passed to the same call as the local function's box. Its access must
// be dynamic too.
@inline(never)
func localFunctionAndNonEscapingClosureCaptureVariable() {
  var x = 0
  @inline(never)
  func outer(_ c: () -> ()) {
    modifyAndCallClosure(&x) { c() }
  }
  outer { x += 1 }
  _blackHole(x)
}

tests.test("LocalFunctionAndNonEscapingClosureCaptureVariable") {
  expectCrashLater(withMessage: "Simultaneous accesses")
  localFunctionAndNonEscapingClosureCaptureVariable()
}

// The local function passes an access through its box, and the non-escaping
// closure, to another call.
@inline(never)
func bump(_ v: inout Int, _ body: () -> ()) {
  body()
  v += 1
}

@inline(never)
func boxPassedWithNonEscapingClosure() {
  var x = 0
  @inline(never)
  func modify(_ body: () -> ()) {
    bump(&x, body)
  }
  modify { x += 1 }
  _blackHole(x)
}

tests.test("BoxPassedWithNonEscapingClosure") {
  expectCrashLater(withMessage: "Simultaneous accesses")
  boxPassedWithNonEscapingClosure()
}

// -----------------------------------------------------------------------------
// No conflict.

// Local functions that only access their captured variables, called directly
// (rdar://157458037).
@inline(never)
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

tests.test("LocalFunctionsWithoutConflict") {
  expectEqual(9, localFunctionsWithoutConflict())
}

// The variable is captured twice by arguments of the same call, but the
// accesses don't overlap.
@inline(never)
func callBoth(_ a: () -> (), _ b: () -> ()) {
  a()
  b()
}

@inline(never)
func localFunctionPassedTwice() -> Int {
  var x = 0
  @inline(never)
  func increment() {
    x += 1
  }
  callBoth(increment, increment)
  return x
}

tests.test("LocalFunctionPassedTwice") {
  expectEqual(2, localFunctionPassedTwice())
}

runAllTests()
