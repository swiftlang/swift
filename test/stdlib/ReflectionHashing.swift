// RUN: %target-build-swift -module-name a %s -o %t.out
// RUN: %target-run %t.out
// REQUIRES: executable_test

// This test expects consistent hash code generation. However String instance
// generate different values on Linux depending on which version of libicu is
// installed. Therefore we run this test only on non-linux OSes, which is
// best described as having objc_interop.

// REQUIRES: objc_interop

//
// This file contains reflection tests that depend on hash values.
// Don't add other tests here.
//

import StdlibUnittest


var Reflection = TestSuite("Reflection")

Reflection.test("Dictionary/Empty") {
  let dict = [Int : Int]()

  var output = ""
  dump(dict, to: &output)

  var expected = "- 0 key/value pairs\n"

  expectEqual(expected, output)
}

Reflection.test("Dictionary") {
  _HashingDetail.fixedSeedOverride = 0xff51afd7ed558ccd
  defer { _HashingDetail.fixedSeedOverride = 0 }
  let dict = ["One": 1, "Two": 2, "Three": 3, "Four": 4, "Five": 5]

  var output = ""
  dump(dict, to: &output)

#if arch(i386) || arch(arm)
  var expected = ""
  expected += "▿ 5 key/value pairs\n"
  expected += "  ▿ (2 elements)\n"
  expected += "    - key: \"Four\"\n"
  expected += "    - value: 4\n"
  expected += "  ▿ (2 elements)\n"
  expected += "    - key: \"One\"\n"
  expected += "    - value: 1\n"
  expected += "  ▿ (2 elements)\n"
  expected += "    - key: \"Two\"\n"
  expected += "    - value: 2\n"
  expected += "  ▿ (2 elements)\n"
  expected += "    - key: \"Five\"\n"
  expected += "    - value: 5\n"
  expected += "  ▿ (2 elements)\n"
  expected += "    - key: \"Three\"\n"
  expected += "    - value: 3\n"
#elseif arch(x86_64) || arch(arm64) || arch(powerpc64) || arch(powerpc64le) || arch(s390x)
  var expected = ""
  expected += "▿ 5 key/value pairs\n"
  expected += "  ▿ (2 elements)\n"
  expected += "    - key: \"Three\"\n"
  expected += "    - value: 3\n"
  expected += "  ▿ (2 elements)\n"
  expected += "    - key: \"Two\"\n"
  expected += "    - value: 2\n"
  expected += "  ▿ (2 elements)\n"
  expected += "    - key: \"Four\"\n"
  expected += "    - value: 4\n"
  expected += "  ▿ (2 elements)\n"
  expected += "    - key: \"One\"\n"
  expected += "    - value: 1\n"
  expected += "  ▿ (2 elements)\n"
  expected += "    - key: \"Five\"\n"
  expected += "    - value: 5\n"
#else
  fatalError("unimplemented")
#endif

  expectEqual(expected, output)
}

Reflection.test("Set") {
  _HashingDetail.fixedSeedOverride = 0xff51afd7ed558ccd
  defer { _HashingDetail.fixedSeedOverride = 0 }
  let s = Set(1...5)

  var output = ""
  dump(s, to: &output)

#if arch(i386) || arch(arm)
  var expected = ""
  expected += "▿ 5 members\n"
  expected += "  - 3\n"
  expected += "  - 1\n"
  expected += "  - 5\n"
  expected += "  - 2\n"
  expected += "  - 4\n"
#elseif arch(x86_64) || arch(arm64) || arch(powerpc64) || arch(powerpc64le) || arch(s390x)
  var expected = ""
  expected += "▿ 5 members\n"
  expected += "  - 5\n"
  expected += "  - 2\n"
  expected += "  - 3\n"
  expected += "  - 4\n"
  expected += "  - 1\n"
#else
  fatalError("unimplemented")
#endif

  expectEqual(expected, output)
}

runAllTests()

