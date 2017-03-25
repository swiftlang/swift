// RUN: %target-run-simple-swift
// RUN: %target-build-swift -O %s -o %t/a.out.optimized
// RUN: %target-run %t/a.out.optimized
// REQUIRES: executable_test

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

let b = string(0x62)
let β = string(0x03_B2)
let 𝔹 = string(0x01_D5_39)

testSuite.test("String literal type") {
  expectEqual(("b" as Expressible<String>).value.description, b)
  expectEqual(("β" as Expressible<String>).value.description, β)
  expectEqual(("𝔹" as Expressible<String>).value.description, 𝔹)
}

testSuite.test("StaticString literal type") {
  expectEqual(("b" as Expressible<StaticString>).value.description, b)
  expectEqual(("β" as Expressible<StaticString>).value.description, β)
  expectEqual(("𝔹" as Expressible<StaticString>).value.description, 𝔹)
}

testSuite.test("Character literal type") {
  expectEqual(("b" as Expressible<Character>).value.description, b)
  expectEqual(("β" as Expressible<Character>).value.description, β)
  expectEqual(("𝔹" as Expressible<Character>).value.description, 𝔹)
}

testSuite.test("UnicodeScalar literal type") {
  expectEqual(("b" as Expressible<UnicodeScalar>).value.description, b)
  expectEqual(("β" as Expressible<UnicodeScalar>).value.description, β)
  expectEqual(("𝔹" as Expressible<UnicodeScalar>).value.description, 𝔹)
}

runAllTests()
