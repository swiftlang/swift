// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out_Onone -enforce-exclusivity=checked -Onone
// RUN: %target-build-swift %s -o %t/a.out_O -enforce-exclusivity=checked -O
// RUN: %target-codesign %t/a.out_Onone
// RUN: %target-codesign %t/a.out_O
// RUN: %target-run %t/a.out_Onone
// RUN: %target-run %t/a.out_O
// REQUIRES: executable_test

// MandatoryAllocBoxToStack only promotes a box, making its accesses static,
// where static enforcement is equivalent to dynamic enforcement. These
// conflicts only happen on some paths, so they must be caught at run time at
// every optimization level. rdar://179406916

import StdlibUnittest

var tests = TestSuite("ExclusivityCapturedBox")

let yes = CommandLine.arguments.count > 0

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

// A local function that captures the variable by box modifies it only on some
// paths.
//
// The local functions are not inlined: after inlining, LegacyDeadObjectElimination
// deletes the dead variable together with its conflicting dynamic accesses.
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

// Local functions that only access their captured box, called directly
// (rdar://157458037). No conflict.
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

// A local function reads on every path and modifies on only some, while a
// read access is in progress.
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

// A local function modifies on every path, but also calls a local function that
// modifies on only some paths. The conflict is always caught at run time.
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

tests.test("LocalFunctionForwardsToMayModifyDuringAccess") {
  expectCrashLater(withMessage: "Simultaneous accesses")
  localFunctionForwardsToMayModifyDuringAccess(!yes)
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

// A local function and an escaping closure that captures the variable are used
// by the same call. Static enforcement can't check accesses through the two
// against each other, so the conflict is caught at run time.
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
