// RUN: %target-build-swift -module-name a %s -o %t.out
// RUN: %target-run %t.out
// REQUIRES: executable_test

// XFAIL: linux

//
// This file contains reflection tests that depend on hash values.
// Don't add other tests here.
//

import StdlibUnittest

var Reflection = TestSuite("Reflection")

Reflection.test("Dictionary/Empty") {
  let dict = [Int : Int]()

  var output = ""
  dump(dict, &output)

  var expected = "- 0 key/value pairs\n"

  expectEqual(expected, output)
}

Reflection.test("Dictionary") {
  let dict = [ "One": 1, "Two": 2, "Three": 3, "Four": 4, "Five": 5 ]

  var output = ""
  dump(dict, &output)

#if arch(i386) || arch(arm)
  var expected = ""
  expected += "▿ 5 key/value pairs\n"
  expected += "  ▿ [0]: (2 elements)\n"
  expected += "    - .0: Four\n"
  expected += "    - .1: 4\n"
  expected += "  ▿ [1]: (2 elements)\n"
  expected += "    - .0: One\n"
  expected += "    - .1: 1\n"
  expected += "  ▿ [2]: (2 elements)\n"
  expected += "    - .0: Two\n"
  expected += "    - .1: 2\n"
  expected += "  ▿ [3]: (2 elements)\n"
  expected += "    - .0: Five\n"
  expected += "    - .1: 5\n"
  expected += "  ▿ [4]: (2 elements)\n"
  expected += "    - .0: Three\n"
  expected += "    - .1: 3\n"
#elseif arch(x86_64) || arch(arm64)
  var expected = ""
  expected += "▿ 5 key/value pairs\n"
  expected += "  ▿ [0]: (2 elements)\n"
  expected += "    - .0: Five\n"
  expected += "    - .1: 5\n"
  expected += "  ▿ [1]: (2 elements)\n"
  expected += "    - .0: Two\n"
  expected += "    - .1: 2\n"
  expected += "  ▿ [2]: (2 elements)\n"
  expected += "    - .0: One\n"
  expected += "    - .1: 1\n"
  expected += "  ▿ [3]: (2 elements)\n"
  expected += "    - .0: Three\n"
  expected += "    - .1: 3\n"
  expected += "  ▿ [4]: (2 elements)\n"
  expected += "    - .0: Four\n"
  expected += "    - .1: 4\n"
#else
  fatalError("unipmelemented")
#endif

  expectEqual(expected, output)
}

Reflection.test("Set") {
  let s = Set(1...5)

  var output = ""
  dump(s, &output)

#if arch(i386) || arch(arm)
  var expected = ""
  expected += "▿ 5 members\n"
  expected += "  - [0]: 3\n"
  expected += "  - [1]: 1\n"
  expected += "  - [2]: 5\n"
  expected += "  - [3]: 2\n"
  expected += "  - [4]: 4\n"
#elseif arch(x86_64) || arch(arm64)
  var expected = ""
  expected += "▿ 5 members\n"
  expected += "  - [0]: 5\n"
  expected += "  - [1]: 2\n"
  expected += "  - [2]: 3\n"
  expected += "  - [3]: 1\n"
  expected += "  - [4]: 4\n"
#else
  fatalError("unimplemented")
#endif

  expectEqual(expected, output)
}

runAllTests()

