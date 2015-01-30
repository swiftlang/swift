// RUN: %target-run-simple-swift

// XFAIL: linux

import SwiftExperimental
import StdlibUnittest

var ExperimentalTestSuite = TestSuite("Experimental")

ExperimentalTestSuite.test("ComposeOperator/SmokeTest") {
  func incr(x: Int) -> Int { return x + 1 }
  func twice(x: Int) -> Int { return x * 2 }

  expectEqual(7, (incr ∘ twice)(3))
}

/*
FIXME: this test crashes SILGen.
<rdar://problem/19150374> Unimplemented: abstraction difference in l-value

ExperimentalTestSuite.test("ComposeOperator/Types") {
  struct A {}
  struct B {}
  struct C {}

  func a(_: A) -> B { return B() }
  func b(_: B) -> C { return C() }

  var result = b ∘ a
  typealias Expected = A -> C
  expectType(Expected.self, &result)
}
*/

/*
FIXME: this test causes an LLVM verifier failure.
<rdar://problem/19150564> LLVM error: invalid linkage type for global declaration
ExperimentalTestSuite.test("ComposeOperator/CountCalls") {
  struct A {}
  struct B {}
  struct C {}

  var aCalled = 0
  var bCalled = 0
  func a(_: A) -> B { ++aCalled; return B() }
  func b(_: B) -> C { ++bCalled; return C() }

  var result = b ∘ a
  expectEqual(0, aCalled)
  expectEqual(0, bCalled)
  result(A())
  expectEqual(1, aCalled)
  expectEqual(1, bCalled)
}
*/

// A modified variant of the test above.
struct A {}
struct B {}
struct C {}

var aCalled = 0
var bCalled = 0
func a(_: A) -> B { ++aCalled; return B() }
func b(_: B) -> C { ++bCalled; return C() }

ExperimentalTestSuite.test("ComposeOperator/CountCalls/Workaround") {
  var result = b ∘ a
  expectEqual(0, aCalled)
  expectEqual(0, bCalled)
  result(A())
  expectEqual(1, aCalled)
  expectEqual(1, bCalled)
}

runAllTests()

