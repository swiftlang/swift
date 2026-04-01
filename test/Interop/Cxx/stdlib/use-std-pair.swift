// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=swift-6)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift -Xcc -std=c++14)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift -Xcc -std=c++17)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift -Xcc -std=c++20)

// REQUIRES: executable_test
// FIXME importing a move-only std::pair into Swift causes a crash in the MoveOnlyChecker, in Linux
// REQUIRES: OS=macosx || OS=windows-msvc

import StdlibUnittest
import StdPair
import CxxStdlib
import Cxx

var StdPairTestSuite = TestSuite("StdPair")

func takePair<T: CxxPair & ~Copyable>(_ _: consuming T) { }

StdPairTestSuite.test("StdPairInts.init") {
  let pi = PairInts(first: 1, second: 2)
  takePair(pi)
  expectEqual(pi.first, 1)
  expectEqual(pi.second, 2)
}

StdPairTestSuite.test("StdPairStrings.init") {
  let ps = PairStrings(first: std.string(), second: std.string())
  expectEqual(ps.first, std.string())
  expectEqual(ps.second, std.string())

  let ps2 = PairStrings(first: std.string("abc"), second: std.string("123"))
  expectEqual(ps2.first, std.string("abc"))
  expectEqual(ps2.second, std.string("123"))
}

StdPairTestSuite.test("StdPair.elements") {
  var pi = getIntPair()
  expectEqual(pi.first, -5)
  expectEqual(pi.second, 12)
  pi.first = 11
  expectEqual(pi.first, 11)
  expectEqual(pi.second, 12)
}

StdPairTestSuite.test("StdPair.ref.elements") {
  let pi = getIntPairPointer().pointee
  expectEqual(pi.first, 4)
  expectEqual(pi.second, 9)
}

StdPairTestSuite.test("PairStructInt.elements") {
  let pair = getPairStructInt(11)
  expectEqual(pair.first.x, 22)
  expectEqual(pair.first.y, -11)
  expectEqual(pair.second, 11)
}

StdPairTestSuite.test("StdPair as CxxPair") {
  func changeFirst(_ p: inout any CxxPair<CInt, CInt>) {
    p.first = 123
  }

  var pair: any CxxPair<CInt, CInt> = getIntPair()
  changeFirst(&pair)
  expectEqual(pair.first, 123)
}

StdPairTestSuite.test("StdPair of NonCopyable") {
  let p1 = NonCopyableFirst(first: NonCopyable(field: 1), second: 2)
  expectEqual(p1.first.field, 1)
  expectEqual(p1.second, 2)
  takePair(p1)

  let p2 = NonCopyableSecond(first: 3, second: NonCopyable(field: 4))
  expectEqual(p2.first, 3)
  expectEqual(p2.second.field, 4)
  takePair(p2)

  let p3 = NonCopyableBoth(first: NonCopyable(field: 5), second: NonCopyable(field: 6))
  expectEqual(p3.first.field, 5)
  expectEqual(p3.second.field, 6)
  takePair(p3)
}

runAllTests()
