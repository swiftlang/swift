// RUN: %target-run-simple-swift
// RUN: %target-build-swift -O %s -o %t/a.out.optimized
// RUN: %target-run %t/a.out.optimized
// REQUIRES: executable_test

import StdlibUnittest

private let testSuite = TestSuite("string literals")

private struct Expressible<T: _ExpressibleByBuiltinStringLiteral>
  : ExpressibleByStringLiteral {
  var value: T
  init(stringLiteral value: T) {
    self.value = value
  }
}

private func string(_ characters: UInt32...) -> String {
  return String(characters.map { Character(string($0)!) })
}

private let b = string(0x62)
private let 🇦🇺 = string(0x1F1E6, 0x1F1FA)
private let abcde = string(0x61, 0x62, 0x63, 0x64, 0x65)

testSuite.test("String literal type") {
  expectEqual(("b" as Expressible<String>).value.description, b)
  expectEqual(("🇦🇺" as Expressible<String>).value.description, 🇦🇺)
  expectEqual(("abcde" as Expressible<String>).value.description, abcde)
}

testSuite.test("StaticString literal type") {
  expectEqual(("b" as Expressible<StaticString>).value.description, b)
  expectEqual(("🇦🇺" as Expressible<StaticString>).value.description, 🇦🇺)
  expectEqual(("abcde" as Expressible<StaticString>).value.description, abcde)
}

runAllTests()
