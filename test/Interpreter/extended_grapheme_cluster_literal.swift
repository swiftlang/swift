// RUN: %target-run-simple-swift
// RUN: %target-build-swift -O %s -o %t/a.out.optimized
// RUN: %target-run %t/a.out.optimized
// REQUIRES: executable_test

import StdlibUnittest

private let testSuite = TestSuite("extendedGraphemeCluster literals")

private struct Expressible
  <T: _ExpressibleByBuiltinExtendedGraphemeClusterLiteral>
  : ExpressibleByExtendedGraphemeClusterLiteral {
  var value: T
  init(extendedGraphemeClusterLiteral value: T) {
    self.value = value
  }
}

private func string(_ characters: UInt32...) -> String {
  return String(characters.map { Character(extendedGraphemeCluster($0)!) })
}

private let b = string(0x62)
private let 🇦🇺 = string(0x1F1E6, 0x1F1FA)
private let 각 = string(0x1100, 0x1161, 0x11A8)

testSuite.test("String literal type") {
  expectEqual(("b" as Expressible<String>).value.description, b)
  expectEqual(("🇦🇺" as Expressible<String>).value.description, 🇦🇺)
  expectEqual(("각" as Expressible<String>).value.description, 각)
}

testSuite.test("StaticString literal type") {
  expectEqual(("b" as Expressible<StaticString>).value.description, b)
  expectEqual(("🇦🇺" as Expressible<StaticString>).value.description, 🇦🇺)
  expectEqual(("각" as Expressible<StaticString>).value.description, 각)
}

testSuite.test("Character literal type") {
  expectEqual(("b" as Expressible<Character>).value.description, b)
  expectEqual(("🇦🇺" as Expressible<Character>).value.description, 🇦🇺)
  expectEqual(("각" as Expressible<Character>).value.description, 각)
}

runAllTests()
