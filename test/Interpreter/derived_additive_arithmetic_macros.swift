// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out -module-name main -Xfrontend -enable-experimental-feature -Xfrontend AdditiveArithmeticDerivedConformances -Xfrontend -enable-experimental-feature -Xfrontend DeriveConformancesViaMacros -Xfrontend -load-plugin-library -Xfrontend %swift-plugin-dir/%target-library-name(SwiftMacros)
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: swift_feature_AdditiveArithmeticDerivedConformances
// REQUIRES: swift_feature_DeriveConformancesViaMacros

import StdlibUnittest

let tests = TestSuite("DerivedAdditiveArithmeticViaMacros")

struct Empty: AdditiveArithmetic {}

struct Int2: AdditiveArithmetic {
  var a: Int
  var b: Int
}

struct Nested: AdditiveArithmetic {
  var int2: Int2
  var int: Int
}

struct Mixed: AdditiveArithmetic {
  var nested: Nested
  var float: Float
  var uint8: UInt8
}

struct Generic<T: AdditiveArithmetic>: AdditiveArithmetic {
  var x: T
  var y: T
}

struct RawIdentifiers: AdditiveArithmetic {
  var `default`: Int
  var `foo bar`: Double
}

struct WithOtherMembers: AdditiveArithmetic {
  var x: Int
  static var scale: Int = 2
  var doubled: Int { x * 2 }
}

struct InExtension {
  var x: Int
}

extension InExtension: AdditiveArithmetic {}

tests.test("a struct without stored properties") {
  expectEqual(Empty(), Empty.zero)
  expectEqual(Empty(), Empty() + Empty())
  expectEqual(Empty(), Empty() - Empty())
}

tests.test("zero is the zero of every stored property") {
  expectEqual(Int2(a: 0, b: 0), Int2.zero)
  expectEqual(Nested(int2: .zero, int: 0), Nested.zero)
  expectEqual(Generic<Double>(x: 0, y: 0), Generic<Double>.zero)
}

tests.test("+ and - are member-wise") {
  let x = Int2(a: 1, b: 2)
  let y = Int2(a: 10, b: 20)
  expectEqual(Int2(a: 11, b: 22), x + y)
  expectEqual(Int2(a: -9, b: -18), x - y)
}

tests.test("nested and mixed stored property types") {
  let nested = Nested(int2: Int2(a: 1, b: 2), int: 3)
  expectEqual(Nested(int2: Int2(a: 2, b: 4), int: 6), nested + nested)
  expectEqual(Nested.zero, nested - nested)

  let mixed = Mixed(nested: nested, float: 0.5, uint8: 1)
  expectEqual(Mixed(nested: nested + nested, float: 1, uint8: 2), mixed + mixed)
  expectEqual(Mixed.zero, mixed - mixed)
}

tests.test("raw identifier stored properties") {
  let x = RawIdentifiers(default: 1, `foo bar`: 2)
  expectEqual(RawIdentifiers(default: 2, `foo bar`: 4), x + x)
  expectEqual(RawIdentifiers.zero, x - x)
}

tests.test("static and computed properties are ignored") {
  let x = WithOtherMembers(x: 1)
  expectEqual(WithOtherMembers(x: 2), x + x)
  expectEqual(4, (x + x).doubled)
}

tests.test("a conformance stated in an extension") {
  let x = InExtension(x: 1)
  expectEqual(InExtension(x: 2), x + x)
  expectEqual(InExtension(x: 0), InExtension.zero)
}

tests.test("through the protocol") {
  func doubleThenHalve<T: AdditiveArithmetic>(_ x: T) -> T {
    var y = T.zero
    y += x
    y += x
    y -= x
    return y
  }

  expectEqual(Int2(a: 1, b: 2), doubleThenHalve(Int2(a: 1, b: 2)))
  expectEqual(Nested.zero, doubleThenHalve(Nested.zero))
  expectEqual(Generic<Float>(x: 1, y: 2), doubleThenHalve(Generic<Float>(x: 1, y: 2)))
}

runAllTests()
