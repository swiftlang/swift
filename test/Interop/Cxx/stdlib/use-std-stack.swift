// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=swift-6)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift -Xcc -std=c++14)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift -Xcc -std=c++17)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift -Xcc -std=c++20)

// Also test this with a bridging header instead of the StdStack module.
// RUN: %empty-directory(%t2)
// RUN: cp %S/Inputs/std-stack.h %t2/std-stack-bridging-header.h
// RUN: %target-run-simple-swift(-D BRIDGING_HEADER -import-objc-header %t2/std-stack-bridging-header.h -cxx-interoperability-mode=swift-6)
// RUN: %target-run-simple-swift(-D BRIDGING_HEADER -import-objc-header %t2/std-stack-bridging-header.h -cxx-interoperability-mode=upcoming-swift)

// REQUIRES: executable_test
//
// Enable this everywhere once we have a solution for modularizing other C++ stdlibs: rdar://87654514
// REQUIRES: OS=macosx || OS=linux-gnu

import StdlibUnittest
#if !BRIDGING_HEADER
import StdStack
#endif
import CxxStdlib

var StdStackTestSuite = TestSuite("StdStack")

StdStackTestSuite.test("stack count") {
  let s1 = initStackOfCInt()
  expectEqual(s1.count, 3)

  let s2 = initEmptyStackOfCInt()
  expectEqual(s2.count, 0)
}

StdStackTestSuite.test("stack isEmpty") {
  let s1 = initStackOfCInt()
  expectFalse(s1.isEmpty)

  let s2 = initEmptyStackOfCInt()
  expectTrue(s2.isEmpty)
}

StdStackTestSuite.test("stack top") {
  let s1 = initStackOfCInt()
  expectEqual(s1.top(), 3)

  let s2 = initEmptyStackOfCInt()
  expectNil(s2.top())
}

StdStackTestSuite.test("stack push") {
  var s1 = initStackOfCInt()

  s1.push(4)
  expectEqual(s1.top(), 4)
  expectEqual(s1.count, 4)
  s1.pop()
  expectEqual(s1.top(), 3)
  expectEqual(s1.count, 3)

  var s2 = initEmptyStackOfCInt()

  s2.push(4)
  expectEqual(s2.top(), 4)
  expectEqual(s2.count, 1)
}

StdStackTestSuite.test("stack pop") {
  var s1 = initStackOfCInt()

  expectEqual(s1.pop(), 3)
  expectEqual(s1.pop(), 2)
  expectEqual(s1.pop(), 1)
  expectNil(s1.pop())
  expectTrue(s1.isEmpty)

  var s2 = initEmptyStackOfCInt()
  expectNil(s2.pop())
}

runAllTests()
