// RUN: %target-run-simple-swift(-I %S/Inputs/ -cxx-interoperability-mode=default)

// REQUIRES: executable_test

import StdlibUnittest
import StaticFactoryAsInit

var Suite = TestSuite("Static factory as initializer")

Suite.test("NonTemplateFactory") {
  let instance = NonTemplateFactory(fromInt: 7)
  expectEqual(7, instance.value)
}

Suite.test("DefaultedTemplateParamFactory") {
  let instance = DefaultedTemplateParamFactory(fromDefaultedTemplate: 8)
  expectEqual(8, instance.value)
}

Suite.test("OverloadedFactories") {
  let one = OverloadedFactories(overload: 3)
  expectEqual(3, one.value)

  let two = OverloadedFactories(overload: 3, extra: 4)
  expectEqual(7, two.value)
}

Suite.test("FactoryAndConstructor") {
  // The real C++ constructor.
  let fromCtor = FactoryAndConstructor(5)
  expectEqual(5, fromCtor.value)

  // The static factory renamed to 'init(fromFactory:)'.
  let fromFactory = FactoryAndConstructor(fromFactory: 6)
  expectEqual(6, fromFactory.value)
}

runAllTests()
