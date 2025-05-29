// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -enable-experimental-cxx-interop -Xfrontend -validate-tbd-against-ir=none -Xfrontend -disable-llvm-verify -g -Xfrontend -disable-availability-checking)
//
// REQUIRES: executable_test

import StdlibUnittest
import POD

extension IntPair {
  static public func swiftMake() -> IntPair { IntPair.create() }
}

struct StructHoldingPair {
  var pair: IntPair
};

class ClassHoldingPair {
  var pair: IntPair

  init(pair: IntPair) { self.pair = pair }
};

var globalPair: IntPair? = nil

var PODTestSuite = TestSuite("Plain old data types that are marked as foreign references")

PODTestSuite.test("Empty") {
  var x = Empty.create()
  expectEqual(x.test(), 42)
  expectEqual(x.testMutable(), 42)

  mutateIt(x)

  x = Empty.create()
  expectEqual(x.test(), 42)
}

PODTestSuite.test("var IntPair") {
  var x = IntPair.create()
  expectEqual(x.test(), 1)
  expectEqual(x.testMutable(), 1)

  mutateIt(x)
  expectEqual(x.test(), 2)
  expectEqual(x.testMutable(), 2)

  x.b = 42
  expectEqual(x.test(), 40)
  expectEqual(x.testMutable(), 40)

  x = IntPair.create()
  expectEqual(x.test(), 1)
}

PODTestSuite.test("static extension") {
  var x = IntPair.swiftMake()
  expectEqual(x.test(), 1)
  expectEqual(x.testMutable(), 1)
}

PODTestSuite.test("let IntPair") {
  let x = IntPair.create()
  expectEqual(x.test(), 1)
  expectEqual(x.testMutable(), 1)

  mutateIt(x)
  expectEqual(x.test(), 2)
  expectEqual(x.testMutable(), 2)

  x.b = 42
  expectEqual(x.test(), 40)
  expectEqual(x.testMutable(), 40)
}

PODTestSuite.test("global") {
  globalPair = IntPair.create()
  expectEqual(globalPair!.test(), 1)
  expectEqual(globalPair!.testMutable(), 1)

  mutateIt(globalPair!)
  expectEqual(globalPair!.test(), 2)
  expectEqual(globalPair!.testMutable(), 2)

  globalPair!.b = 42
  expectEqual(globalPair!.test(), 40)
  expectEqual(globalPair!.testMutable(), 40)

  globalPair = IntPair.create()
  expectEqual(globalPair!.test(), 1)
}

PODTestSuite.test("RefHoldingPairRef") {
  var x = RefHoldingPairRef.create()
  expectEqual(x.test(), 41)
  expectEqual(x.testMutable(), 41)

  x.pair.b = 42
  expectEqual(x.test(), 1)
  expectEqual(x.testMutable(), 1)

  x = RefHoldingPairRef.create()
  expectEqual(x.test(), 41)
}

PODTestSuite.test("RefHoldingPairPtr") {
  var x = RefHoldingPairPtr.create()
  expectEqual(x.test(), 41)
  expectEqual(x.testMutable(), 41)

  x.pair.b = 42
  expectEqual(x.test(), 1)
  expectEqual(x.testMutable(), 1)

  x = RefHoldingPairPtr.create()
  expectEqual(x.test(), 41)
}

PODTestSuite.test("ValueHoldingPairRef") {
  let x = ValueHoldingPairRef()
  expectEqual(x.pair.test(), 1)

  let pair2 = IntPair.create()
  pair2.b = 123
  expectEqual(x.sub(pair2), -121)
  expectEqual(x.max(pair2).test(), pair2.test())
}

PODTestSuite.test("StructHoldingPair") {
  var x = StructHoldingPair(pair: IntPair.create())
  expectEqual(x.pair.test(), 1)
  expectEqual(x.pair.testMutable(), 1)

  mutateIt(x.pair)
  expectEqual(x.pair.test(), 2)
  expectEqual(x.pair.testMutable(), 2)

  x.pair = IntPair.create()
  expectEqual(x.pair.test(), 1)
}

PODTestSuite.test("ClassHoldingPair") {
  var x = ClassHoldingPair(pair: IntPair.create())
  expectEqual(x.pair.test(), 1)
  expectEqual(x.pair.testMutable(), 1)

  mutateIt(x.pair)
  expectEqual(x.pair.test(), 2)
  expectEqual(x.pair.testMutable(), 2)

  x.pair = IntPair.create()
  expectEqual(x.pair.test(), 1)
}

PODTestSuite.test("BigType") {
  var x = BigType.create()
  expectEqual(x.test(), 1)
  expectEqual(x.testMutable(), 1)

  mutateIt(x)
  expectEqual(x.test(), 2)
  expectEqual(x.testMutable(), 2)

  x.b = 42
  expectEqual(x.test(), 40)
  expectEqual(x.testMutable(), 40)

  x = BigType.create()
  expectEqual(x.test(), 1)
}

PODTestSuite.test("DerivedRef") {
  var x = DerivedRef.create()
  expectEqual(x.test(), 1)
  expectEqual(x.testMutating(), 1)
  expectEqual(x.testDerived(), 2)
  expectEqual(x.testDerivedMutating(), 2)
}

PODTestSuite.test("BaseRef") {
  var x = BaseRef.create()
  expectEqual(x.test(), 1)
  expectEqual(x.testMutating(), 1)
}

runAllTests()
