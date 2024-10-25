// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=swift-6)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift -Xcc -std=c++17)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift -Xcc -std=c++20)

// REQUIRES: executable_test

import StdlibUnittest
import CxxStdlib
import StdStringView

var StdStringTestSuite = TestSuite("StdStringView")

StdStringTestSuite.test("String.init(_: std.string_view)") {
    expectEqual("abc210", String(staticStringView))
    expectEqual("", String(staticEmptyStringView))
    expectEqual("тест", String(staticNonASCIIStringView))
}

StdStringTestSuite.test("String.init(_: std.u16string_view)") {
    expectEqual("abc210", String(staticU16StringView))
    expectEqual("", String(staticU16EmptyStringView))
    expectEqual("тест", String(staticU16NonASCIIStringView))
}

StdStringTestSuite.test("String.init(_: std.u32string_view)") {
    expectEqual("abc210", String(staticU32StringView))
    expectEqual("", String(staticU32EmptyStringView))
    expectEqual("тест", String(staticU32NonASCIIStringView))
}

runAllTests()
