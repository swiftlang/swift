// RUN: %target-run-simple-swift(-enable-experimental-feature PackExpansionInArrayLiterals -target %target-swift-5.9-abi-triple)
// RUN: %target-run-simple-swift(-O -enable-experimental-feature PackExpansionInArrayLiterals -target %target-swift-5.9-abi-triple)

// REQUIRES: executable_test
// REQUIRES: swift_feature_PackExpansionInArrayLiterals

import StdlibUnittest

var arrays = TestSuite("VariadicGenericArrayLiterals")

func toAny<each T>(_ t: repeat each T) -> [Any] {
  return [repeat each t]
}

func mixed<each T, each U>(_ t: repeat each T, u: repeat each U) -> [Any] {
  return [0, repeat each t, "middle", repeat each u, 1.0]
}

func descriptions<each T: CustomStringConvertible>(_ t: repeat each T) -> [String] {
  return [repeat (each t).description]
}

arrays.test("Basic") {
  expectEqual(0, toAny().count)
  expectEqual(["1"], toAny(1).map { "\($0)" })
  expectEqual(["1", "a", "true"], toAny(1, "a", true).map { "\($0)" })
  expectEqual(["1", "2.0", "x"], descriptions(1, 2.0, "x"))
  expectEqual([], descriptions())
}

arrays.test("Mixed") {
  expectEqual(["0", "middle", "1.0"], mixed().map { "\($0)" })
  expectEqual(["0", "1", "middle", "1.0"], mixed(1).map { "\($0)" })
  expectEqual(["0", "middle", "a", "b", "1.0"], mixed(u: "a", "b").map { "\($0)" })
  expectEqual(["0", "1", "2", "middle", "a", "1.0"],
              mixed(1, 2, u: "a").map { "\($0)" })
}

func closures<each T: BinaryInteger>(_ t: repeat each T) -> [(Int) -> Int] {
  return [repeat { $0 + Int(each t) }]
}

arrays.test("Reabstraction") {
  expectEqual([11, 12, 13], closures(1, Int8(2), UInt(3)).map { $0(10) })
}

struct MyList: ExpressibleByArrayLiteral {
  var elements: [String]
  init(arrayLiteral elements: String...) { self.elements = elements }
}

func custom<each T>(_ t: repeat each T) -> MyList {
  return ["<", repeat String(describing: each t), ">"]
}

arrays.test("ExpressibleByArrayLiteral") {
  expectEqual(["<", ">"], custom().elements)
  expectEqual(["<", "1", "true", ">"], custom(1, true).elements)
}

struct Failure: Error, Equatable {}

func make<T>(_ t: T, failAt: Int, index: inout Int) throws -> LifetimeTracked {
  defer { index += 1 }
  if index == failAt { throw Failure() }
  return LifetimeTracked(index)
}

// Elements are numbered in evaluation order. Failing at 0..<3 fails within
// the expansion; failing at 3 fails in the scalar element after it, once the
// whole expansion has been initialized.
func throwing<each T>(failAt: Int, _ t: repeat each T) throws -> [LifetimeTracked] {
  var index = 0
  return [LifetimeTracked(100),
          repeat try make(each t, failAt: failAt, index: &index),
          try make((), failAt: failAt, index: &index)]
}

// Failing at 0..<2 fails within the first expansion; failing at 2..<5 fails
// within the second one, once the first one has been initialized.
func throwingTwo<each T, each U>(
  failAt: Int, _ t: repeat each T, u: repeat each U
) throws -> [LifetimeTracked] {
  var index = 0
  return [repeat try make(each t, failAt: failAt, index: &index),
          repeat try make(each u, failAt: failAt, index: &index)]
}

arrays.test("Cleanups") {
  for failAt in 0..<4 {
    expectThrows(Failure(), { _ = try throwing(failAt: failAt, 1, "a", 2.0) })
  }
  let result = try! throwing(failAt: 4, 1, "a", 2.0)
  expectEqual([100, 0, 1, 2, 3], result.map { $0.value })

  for failAt in 0..<5 {
    expectThrows(Failure(), {
      _ = try throwingTwo(failAt: failAt, 1, "a", u: 2.0, true, ())
    })
  }
  let two = try! throwingTwo(failAt: 5, 1, "a", u: 2.0, true, ())
  expectEqual([0, 1, 2, 3, 4], two.map { $0.value })
}

runAllTests()
