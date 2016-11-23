// RUN: %target-run-simple-swift
// RUN: %target-build-swift -O %s -o %t/a.out.optimized
// RUN: %target-run %t/a.out.optimized
// REQUIRES: executable_test

import StdlibUnittest

let tupleCastTests = TestSuite("Tuple casting")

func anyToIntPoint(_ x: Any) -> (x: Int, y: Int) {
  return x as! (x: Int, y: Int)
}

func anyToIntPointOpt(_ x: Any) -> (x: Int, y: Int)? {
  return x as? (x: Int, y: Int)
}

func anyToInt2(_ x: Any) -> (Int, Int) {
  return x as! (Int, Int)
}

func anyToPartlyLabeled(_ x: Any) -> (first: Int, Int, third: Int) {
  return x as! (first: Int, Int, third: Int)
}

tupleCastTests.test("Adding/removing labels") {
  expectEqual("(x: 1, y: 2)",
              String(describing: anyToIntPoint((x: 1, y: 2))))
  expectEqual("(x: 3, y: 4)",
              String(describing: anyToIntPoint((3, 4))))
  expectEqual("(x: 5, y: 6)",
              String(describing: anyToIntPoint((x: 5, 6))))

  expectEqual("(1, 2)", String(describing: anyToInt2((1, 2))))
  expectEqual("(3, 4)", String(describing: anyToInt2((x: 3, y: 4))))
  expectEqual("(5, 6)", String(describing: anyToInt2((x: 5, 6))))

  expectEqual("(first: 1, 2, third: 3)",
              String(describing: anyToPartlyLabeled((1, 2, 3))))
}

tupleCastTests.test("Incorrect labels conditional cast") {
  expectNil(anyToIntPointOpt((x: 1, z: 2)))
  expectEqual("Optional((x: 1, y: 2))",
              String(describing: anyToIntPointOpt((x: 1, y: 2))))
}

tupleCastTests
  .test("Incorrect labels forced cast")
  .crashOutputMatches("Could not cast value of type '(x : Swift.Int, z : Swift.Int)'")
  .code {
  expectCrashLater()
  _ = anyToIntPoint((x: 1, z: 2))
}

func castToThree<T, U, V>(_ x: Any, _: T.Type, _: U.Type, _: V.Type)
    -> (t: T, u: U, v: V) {
  return x as! (t: T, u: U, v: V)
}

func castToThreeOpt<T, U, V>(_ x: Any, _: T.Type, _: U.Type, _: V.Type)
    -> (t: T, u: U, v: V)? {
  return x as? (t: T, u: U, v: V)
}

class LifetimeA {
  var tracked: LifetimeTracked

  init(value: Int) { 
    tracked = LifetimeTracked(value)
  }
}

class LifetimeB : LifetimeA {
}

class LifetimeC : LifetimeA {
}

protocol P { }
extension LifetimeA : P { }

tupleCastTests.test("Elementwise tuple casts that succeed") {
  let abc: (P, Any, P) = (LifetimeA(value: 1),
                          LifetimeB(value: 2),
                          LifetimeC(value: 3))

  expectEqual(
    "(t: main.LifetimeA, u: main.LifetimeB, v: main.LifetimeC)",
    String(describing: castToThree(abc, LifetimeA.self, LifetimeA.self,
                                   LifetimeA.self)))

  expectEqual(
    "(t: main.LifetimeA, u: main.LifetimeB, v: main.LifetimeC)",
    String(describing: castToThree((LifetimeA(value: 1),
                                    LifetimeB(value: 2),
                                    LifetimeC(value: 3)) as (P, Any, P),
                                   LifetimeA.self, LifetimeA.self,
                                   LifetimeA.self)))
}

tupleCastTests.test("Elementwise tuple casts that conditionally fail") {
  let abc: (P, Any, P) = (LifetimeA(value: 1),
                          LifetimeB(value: 2),
                          LifetimeC(value: 3))
  expectNil(castToThreeOpt(abc, LifetimeA.self, LifetimeB.self, LifetimeB.self))
  expectNil(castToThreeOpt(abc, LifetimeA.self, LifetimeC.self, LifetimeC.self))
  expectNil(castToThreeOpt(abc, LifetimeC.self, LifetimeB.self, LifetimeC.self))
}

tupleCastTests
  .test("Elementwise tuple casts that crash (1/3)")
  .crashOutputMatches("Could not cast value of type 'main.LifetimeA'")
  .code {
  let abc: (P, Any, P) = (LifetimeA(value: 1),
                          LifetimeB(value: 2),
                          LifetimeC(value: 3))
  expectCrashLater()
  _ = castToThree(abc, LifetimeC.self, LifetimeB.self, LifetimeC.self)
}

tupleCastTests
  .test("Elementwise tuple casts that crash (2/3)")
  .crashOutputMatches("Could not cast value of type 'main.LifetimeB")
  .code {
  let abc: (P, Any, P) = (LifetimeA(value: 1),
                          LifetimeB(value: 2),
                          LifetimeC(value: 3))
  expectCrashLater()
  _ = castToThree(abc, LifetimeA.self, LifetimeC.self, LifetimeC.self)
}

tupleCastTests
  .test("Elementwise tuple casts that crash (3/3)")
  .crashOutputMatches("Could not cast value of type 'main.LifetimeC")
  .code {
  let abc: (P, Any, P) = (LifetimeA(value: 1),
                          LifetimeB(value: 2),
                          LifetimeC(value: 3))
  expectCrashLater()
  _ = castToThree(abc, LifetimeA.self, LifetimeB.self, LifetimeB.self)
}

runAllTests()
