// RUN: %target-run-simple-swift(-Xcc -stdlib=libc++ -I %S/Inputs -cxx-interoperability-mode=swift-5.9)
// RUN: %target-run-simple-swift(-Xcc -stdlib=libc++ -I %S/Inputs -cxx-interoperability-mode=swift-6)
// RUN: %target-run-simple-swift(-Xcc -stdlib=libc++ -I %S/Inputs -cxx-interoperability-mode=upcoming-swift)
// RUN: %target-run-simple-swift(-Xcc -stdlib=libc++ -I %S/Inputs -cxx-interoperability-mode=upcoming-swift -Xcc -std=c++14)
// RUN: %target-run-simple-swift(-Xcc -stdlib=libc++ -I %S/Inputs -cxx-interoperability-mode=upcoming-swift -Xcc -std=c++17)
// RUN: %target-run-simple-swift(-Xcc -stdlib=libc++ -I %S/Inputs -cxx-interoperability-mode=upcoming-swift -Xcc -std=c++20)

// REQUIRES: OS=linux-gnu
// REQUIRES: system_wide_libcxx

// This test ensures that the C++ stdlib functionality can be used when
// compiling with libc++ on Linux. Since the CxxStdlib overlay currently isn't
// available when compiling with a non-default C++ stdlib, this only tests the
// features that don't require CxxStdlib overlay.

// This test should be removed once CxxStdlib overlay supports libc++ on Linux.
// Instead, we should run the existing use-std-*.swift tests with libc++ in
// addition to the default config.

import StdlibUnittest
import CxxStdlib
import StdVector

var LibcxxLinuxTestSuite = TestSuite("libc++-Linux")

LibcxxLinuxTestSuite.test("std::string") {
  var s = std.string()
  s.push_back(65)
  s.push_back(66)
  s.push_back(67)
  expectEqual(3, s.size())

  var sum = 0
  for c in s { // relies on the conformance to CxxRandomAccessCollection
    sum += Int(c)
  }
  expectEqual(198, sum)
}

LibcxxLinuxTestSuite.test("std::vector<std::string>") {
  var v = VectorOfString()
  v.push_back(std.string())
  v.push_back(std.string())
  expectEqual(2, v.size())
  
  var count = 0
  for s in v { // relies on the conformance to CxxRandomAccessCollection
    count += 1
  }
  expectEqual(2, count)
}

runAllTests()
