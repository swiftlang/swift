// RUN: %target-run-simple-swift 2>&1 | FileCheck %s

import StdlibUnittest
import Darwin // for getchar()

var StdinTestSuite = TestSuite("Stdin")

StdinTestSuite.test("passes1")
  .stdin("")
  .code {
}

StdinTestSuite.test("passes2")
  .stdin("\n")
  .code {
  let line = _stdlib_getline()
  expectOptionalEqual("", line)
}

StdinTestSuite.test("passes3")
  .stdin("abc\n")
  .code {
  let line = _stdlib_getline()
  expectOptionalEqual("abc", line)
}

StdinTestSuite.test("passes3")
  .stdin("abc\ndefghi\n")
  .code {
  let line = _stdlib_getline()
  expectOptionalEqual("abc", line)
  expectOptionalEqual("defghi", line)
}

runAllTests()

