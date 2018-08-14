// RUN: %empty-directory(%t)

// RUN: %target-clang -fobjc-arc %S/Inputs/BlockGlobals/BlockGlobals.m -c -o %t/BlockGlobals.o
// RUN: %target-build-swift %s -import-objc-header %S/Inputs/BlockGlobals/BlockGlobals.h %t/BlockGlobals.o -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main

// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation
import StdlibUnittest

var BlockGlobalsTestSuite = TestSuite("BlockGlobals")

BlockGlobalsTestSuite.test("const block") {
  expectEqual((@convention(block) (String) -> String).self as Any.Type, type(of: constBlockGlobal))
  expectEqual("default const block: abc", constBlockGlobal("abc"))
}

BlockGlobalsTestSuite.test("const function pointer") {
  expectEqual((@convention(c) (String) -> String).self as Any.Type, type(of: constFPGlobal))
  expectEqual("default const FP: abc", constFPGlobal("abc"))
}

// FIXME: Add tests for mutable globals, including mutating them, once the
// compiler supports it, as well as loading from the const globals without
// immediately calling them.

runAllTests()
