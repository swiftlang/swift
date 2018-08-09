// RUN: %target-run-simple-swift
// RUN: %target-build-swift -O %s -o %t/a.out.optimized
// RUN: %target-codesign %t/a.out.optimized
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
  return String(characters.map { Character(UnicodeScalar($0)!) })
}
private func expressible<T>(_ literal: Expressible<T>, as type: T.Type)
  -> String where T: CustomStringConvertible {
  return literal.value.description
}

private let b = string(0x62)
private let 🇦🇺 = string(0x1F1E6, 0x1F1FA)
private let 각 = string(0x1100, 0x1161, 0x11A8)

testSuite.test("String literal type") {
  expectEqual(expressible("b", as: String.self), b)
  expectEqual(expressible("🇦🇺", as: String.self), 🇦🇺)
  expectEqual(expressible("각", as: String.self), 각)
}

testSuite.test("StaticString literal type") {
  expectEqual(expressible("b", as: StaticString.self), b)
  expectEqual(expressible("🇦🇺", as: StaticString.self), 🇦🇺)
  expectEqual(expressible("각", as: StaticString.self), 각)
}

testSuite.test("Character literal type") {
  expectEqual(expressible("b", as: Character.self), b)
  expectEqual(expressible("🇦🇺", as: Character.self), 🇦🇺)
  expectEqual(expressible("각", as: Character.self), 각)
}

runAllTests()
