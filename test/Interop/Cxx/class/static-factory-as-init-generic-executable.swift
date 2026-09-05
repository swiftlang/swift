// RUN: %target-run-simple-swift(-I %S/Inputs/ -cxx-interoperability-mode=default)

// REQUIRES: executable_test

import StdlibUnittest
import StaticFactoryAsInitGeneric

var Suite = TestSuite("Generic static factory as initializer")

Suite.test("GenericFactory") {
  // T == int
  let fromInt = GenericFactory(fromGeneric: 1 as CInt)
  expectEqual(1, fromInt.value)

  // T == double; the factory truncates via static_cast<int>.
  let fromDouble = GenericFactory(fromGeneric: 2.75 as Double)
  expectEqual(2, fromDouble.value)
}

Suite.test("MultiGenericFactory") {
  // T == U == int
  let bothInt = MultiGenericFactory(2 as CInt, other: 3 as CInt)
  expectEqual(5, bothInt.value)

  // T == int, U == double; the factory truncates via static_cast<int>.
  let mixed = MultiGenericFactory(2 as CInt, other: 4.5 as Double)
  expectEqual(6, mixed.value)
}

Suite.test("MixedGenericFactory") {
  // T == int, alongside a concrete int parameter.
  let fromInt = MixedGenericFactory(generic: 4 as CInt, concrete: 5)
  expectEqual(9, fromInt.value)

  // T == double, alongside the same concrete int parameter.
  let fromDouble = MixedGenericFactory(generic: 6.5 as Double, concrete: 5)
  expectEqual(11, fromDouble.value)
}

runAllTests()
