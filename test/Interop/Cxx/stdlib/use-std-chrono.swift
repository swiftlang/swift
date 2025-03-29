// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=swift-5.9)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=swift-6)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift -Xcc -std=c++14)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift -Xcc -std=c++17)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift -Xcc -std=c++20)
// TODO once macOS CI supports C++23: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift -Xcc -std=c++23)

// REQUIRES: executable_test

import StdlibUnittest
import CxxStdlib

var StdUniquePtrTestSuite = TestSuite("StdChrono")

if #available(SwiftStdlib 5.7, *) {
  StdUniquePtrTestSuite.test("std::chrono::seconds <=> Duration") {
    let d1 = Duration.seconds(123)
    let c1 = std.chrono.seconds(d1)
    expectEqual(123, c1.count())
    expectEqual(d1, Duration(c1))

    let d2 = Duration.milliseconds(1)
    let c2 = std.chrono.seconds(d2)
    expectEqual(0, c2.count())

    let d3 = Duration.milliseconds(5000)
    let c3 = std.chrono.seconds(d3)
    expectEqual(5, c3.count())
    expectEqual(d3, Duration(c3))

    let d4 = Duration.seconds(-123)
    let c4 = std.chrono.seconds(d4)
    expectEqual(-123, c4.count())
    expectEqual(d4, Duration(c4))

    let d5 = Duration.milliseconds(-5000)
    let c5 = std.chrono.seconds(d5)
    expectEqual(-5, c5.count())
    expectEqual(d5, Duration(c5))
  }

  StdUniquePtrTestSuite.test("std::chrono::milliseconds <=> Duration") {
    let d1 = Duration.milliseconds(321)
    let c1 = std.chrono.milliseconds(d1)
    expectEqual(321, c1.count())
    expectEqual(d1, Duration(c1))

    let d2 = Duration.microseconds(1)
    let c2 = std.chrono.milliseconds(d2)
    expectEqual(0, c2.count())

    let d3 = Duration.microseconds(2000)
    let c3 = std.chrono.milliseconds(d3)
    expectEqual(2, c3.count())
    expectEqual(d3, Duration(c3))

    let d4 = Duration.seconds(25)
    let c4 = std.chrono.milliseconds(d4)
    expectEqual(25000, c4.count())
    expectEqual(d4, Duration(c4))

    let d5 = Duration.microseconds(-5000)
    let c5 = std.chrono.milliseconds(d5)
    expectEqual(-5, c5.count())
    expectEqual(d5, Duration(c5))
  }

  StdUniquePtrTestSuite.test("std::chrono::microseconds from Duration") {
    let d1 = Duration.microseconds(456)
    let c1 = std.chrono.microseconds(d1)
    expectEqual(456, c1.count())
    expectEqual(d1, Duration(c1))

    let d2 = Duration.nanoseconds(1)
    let c2 = std.chrono.milliseconds(d2)
    expectEqual(0, c2.count())

    let d3 = Duration.seconds(5)
    let c3 = std.chrono.microseconds(d3)
    expectEqual(5000000, c3.count())
    expectEqual(d3, Duration(c3))

    let d4 = Duration.milliseconds(5)
    let c4 = std.chrono.microseconds(d4)
    expectEqual(5000, c4.count())
    expectEqual(d4, Duration(c4))

    let d5 = Duration.microseconds(-654)
    let c5 = std.chrono.microseconds(d5)
    expectEqual(-654, c5.count())
    expectEqual(d5, Duration(c5))
  }

  StdUniquePtrTestSuite.test("std::chrono::nanoseconds from Duration") {
    let d1 = Duration.nanoseconds(789)
    let c1 = std.chrono.nanoseconds(d1)
    expectEqual(789, c1.count())
    expectEqual(d1, Duration(c1))

    let d2 = Duration.nanoseconds(1) / 1000
    let c2 = std.chrono.nanoseconds(d2)
    expectEqual(0, c2.count())

    let d3 = Duration.seconds(5)
    let c3 = std.chrono.nanoseconds(d3)
    expectEqual(5000000000, c3.count())
    expectEqual(d3, Duration(c3))

    let d4 = Duration.milliseconds(2)
    let c4 = std.chrono.nanoseconds(d4)
    expectEqual(2000000, c4.count())
    expectEqual(d4, Duration(c4))

    let d5 = Duration.nanoseconds(-654)
    let c5 = std.chrono.nanoseconds(d5)
    expectEqual(-654, c5.count())
    expectEqual(d5, Duration(c5))
  }
}

runAllTests()
