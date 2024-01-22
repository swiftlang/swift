// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop)
// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop -D USE_CUSTOM_STRING_API)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift -D SUPPORTS_DEFAULT_ARGUMENTS -D USE_CUSTOM_STRING_API)
//
// REQUIRES: executable_test

import StdlibUnittest
import CxxStdlib
#if USE_CUSTOM_STRING_API
import StdString
#endif

var StdStringTestSuite = TestSuite("StdString")

StdStringTestSuite.test("init") {
    let s = std.string()
    expectEqual(s.size(), 0)
    expectTrue(s.empty())
}

StdStringTestSuite.test("push back") {
    var s = std.string()
    s.push_back(42)
    expectEqual(s.size(), 1)
    expectFalse(s.empty())
    expectEqual(s[0], 42)
}

#if USE_CUSTOM_STRING_API
StdStringTestSuite.test("get from a method") {
    let box = HasMethodThatReturnsString()
    let str = box.getString()
    expectEqual(str.size(), 3)
    expectEqual(str, std.string("111"))
}

StdStringTestSuite.test("pass as an argument") {
    let s = std.string("a")
    let res = takesStringWithDefaultArg(s)
    expectEqual(res.size(), 1)
    expectEqual(res[0], 97)
}

#if SUPPORTS_DEFAULT_ARGUMENTS
StdStringTestSuite.test("pass as a default argument") {
    let res = takesStringWithDefaultArg()
    expectEqual(res.size(), 3)
    expectEqual(res[0], 97)
    expectEqual(res[1], 98)
    expectEqual(res[2], 99)
}

StdStringTestSuite.test("pass as a const ref default argument") {
    let res = takesStringConstRefWithDefaultArg()
    expectEqual(res.size(), 3)
    expectEqual(res[0], 100)
    expectEqual(res[1], 101)
    expectEqual(res[2], 102)
}
#endif
#endif

runAllTests()
