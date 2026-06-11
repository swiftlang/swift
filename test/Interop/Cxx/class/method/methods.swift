// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -enable-experimental-cxx-interop)
//
// REQUIRES: executable_test

import StdlibUnittest
import Methods

var CxxMethodTestSuite = TestSuite("CxxMethods")

CxxMethodTestSuite.test("() -> Void") {
  var instance = HasMethods()

  instance.nonConstMethod()
  instance.nonConstMethod(5)
  HasMethods.nonConstMethod(4.2) // Testing name collision
  instance.constMethod()
  HasMethods.constMethod(4.2) // Testing name collision
}

CxxMethodTestSuite.test("(Int) -> Int") {
  var instance = HasMethods()

  expectEqual(42, instance.nonConstPassThrough(42))
  expectEqual(42, instance.constPassThrough(42))
}

CxxMethodTestSuite.test("(Int, Int) -> Int") {
  var instance = HasMethods()

  expectEqual(42, instance.nonConstSum(40, 2))
  expectEqual(42, instance.constSum(40, 2))
}

CxxMethodTestSuite.test("(NonTrivialInWrapper, NonTrivialInWrapper) -> Int") {
  var instance = HasMethods()

  expectEqual(42, instance.nonConstSum(NonTrivialInWrapper(40), NonTrivialInWrapper(2)))
  expectEqual(42, instance.constSum(NonTrivialInWrapper(40), NonTrivialInWrapper(2)))
}

CxxMethodTestSuite.test("(NonTrivialInWrapper, NonTrivialInWrapper) -> NonTrivialInWrapper") {
  var instance = HasMethods()

  expectEqual(42, instance.nonConstSumAsWrapper(NonTrivialInWrapper(40), NonTrivialInWrapper(2)).value)
  expectEqual(42, instance.constSumAsWrapper(NonTrivialInWrapper(40), NonTrivialInWrapper(2)).value)
}

CxxMethodTestSuite.test("(Int) -> NonTrivialInWrapper") {
  var instance = HasMethods()

  expectEqual(42, instance.nonConstPassThroughAsWrapper(42).value)
  expectEqual(42, instance.constPassThroughAsWrapper(42).value)
}

CxxMethodTestSuite.test("Constructor with ref params") {
  let a = CInt(42)
  let b = CInt(11)
  var instance = ReferenceParams(a, b)

  expectEqual(42, instance.a)
  expectEqual(11, instance.b)
}

// Just make sure we don't crash
CxxMethodTestSuite.test("Static method with ref params") {
  let a = CInt(42)
  let b = CInt(11)
  ReferenceParams.staticMethod(a, b)
}

CxxMethodTestSuite.test("C++ method called init") {
  var instance = HasInitMethods(field: 42)
  expectEqual(instance.`init`(), 42)
  expectEqual(instance.initMutating(7), 7)
}

CxxMethodTestSuite.test("C++ method with swift_name `init`") {
  var instance = HasInitWithBackticks(x: 3)
  expectEqual(instance.`init`(), 3)
}

CxxMethodTestSuite.test("C++ method called init, renamed with swift_name attribute") {
  var instance = HasRenamedInitMethods(field: 42)
  expectEqual(instance.start(), 42)
  expectEqual(instance.startWith(7), 49)
}

CxxMethodTestSuite.test("C++ static init method still gets imported as an initializer") {
  let instance = HasStaticInitFactoryAndInitMethod(value: 42)
  expectEqual(instance.`init`(), 45)
}

CxxMethodTestSuite.test("C++ static init method that is not an initializer") {
  expectEqual(HasNonInitializerStaticInitMethod.nonInitializer(42), 42)
}

CxxMethodTestSuite.test("C++ constructor with swift_name attribute") {
  let instance = ConstructorWithRenamedLabel(renamed: 42)
  expectEqual(instance.value, 42)
}

CxxMethodTestSuite.test("Inline factory function renamed to init") {
  let instance = WithFactory(x: 5)
  expectEqual(instance.x, 5)

  let anotherInstance = WithFactory(n: 5)
  expectEqual(anotherInstance.x, 10)
}

runAllTests()
