// RUN: %target-run-simple-swift
// RUN: %target-build-swift -O %s -o %t/a.out.optimized
// RUN: %target-codesign %t/a.out.optimized
// RUN: %target-run %t/a.out.optimized
// REQUIRES: executable_test
// XFAIL: swift_test_mode_optimize_none_with_opaque_values

import StdlibUnittest

private let testSuite = TestSuite("UnicodeScalar literals")

private struct Expressible<T: _ExpressibleByBuiltinUnicodeScalarLiteral>
  : ExpressibleByUnicodeScalarLiteral {
  var value: T
  init(unicodeScalarLiteral value: T) {
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

let b = string(0x62)
let β = string(0x03_B2)
let 𝔹 = string(0x01_D5_39)

testSuite.test("String literal type") {
  expectEqual(expressible("b", as: String.self), b)
  expectEqual(expressible("β", as: String.self), β)
  expectEqual(expressible("𝔹", as: String.self), 𝔹)
}

testSuite.test("StaticString literal type") {
  expectEqual(expressible("b", as: StaticString.self), b)
  expectEqual(expressible("β", as: StaticString.self), β)
  expectEqual(expressible("𝔹", as: StaticString.self), 𝔹)
}

testSuite.test("Character literal type") {
  expectEqual(expressible("b", as: Character.self), b)
  expectEqual(expressible("β", as: Character.self), β)
  expectEqual(expressible("𝔹", as: Character.self), 𝔹)
}

testSuite.test("UnicodeScalar literal type") {
  expectEqual(expressible("b", as: UnicodeScalar.self), b)
  expectEqual(expressible("β", as: UnicodeScalar.self), β)
  expectEqual(expressible("𝔹", as: UnicodeScalar.self), 𝔹)
}

runAllTests()
