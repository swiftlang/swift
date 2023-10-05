// RUN: %target-run-simple-swift(-O -g -o %t/dead_code_elimination_access  -Xfrontend -enable-experimental-move-only)

// REQUIRES: executable_test

import StdlibUnittest

class Foo {}

var global = Foo()

@inline(never)
func useFoo(x: Foo) {
  global = Foo()
}

@inline(never)
func callUseFoo() {
  useFoo(x: _borrow global)
}

var DeadExclusivityTest = TestSuite("DeadExclusivityTest")

DeadExclusivityTest.test("deadAccessScope") {
  expectCrashLater(withMessage: "Fatal access conflict detected")
  callUseFoo()
}

runAllTests()
