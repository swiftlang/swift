// RUN: %target-run-simple-swift

import StdlibUnittest
import DifferentiationUnittest

var ArrayAutoDiffTests = TestSuite("ArrayAutoDiff")

typealias FloatArrayTan = Array<Float>.TangentVector
typealias TrackedFloatArrayTan = Array<Tracked<Float>>.TangentVector

ArrayAutoDiffTests.test("ArrayIdentity") {
  func arrayIdentity(_ x: [Float]) -> [Float] {
    return x
  }

  let backprop = pullback(at: [5, 6, 7, 8], in: arrayIdentity)
  expectEqual(
    FloatArrayTan([1, 2, 3, 4]),
    backprop(FloatArrayTan([1, 2, 3, 4])))
}

ArrayAutoDiffTests.test("ArraySubscript") {
  func sumFirstThree(_ array: [Float]) -> Float {
    return array[0] + array[1] + array[2]
  }

  expectEqual(
    FloatArrayTan([1, 1, 1, 0, 0, 0]),
    gradient(at: [2, 3, 4, 5, 6, 7], in: sumFirstThree))
}

ArrayAutoDiffTests.testWithLeakChecking("ArrayLiteral") {
  do {
    func twoElementLiteral(_ x: Tracked<Float>, _ y: Tracked<Float>) -> [Tracked<Float>] {
      return [x, y]
    }
    let pb = pullback(at: 1, 1, in: twoElementLiteral)
    expectEqual((1, 2), pb(TrackedFloatArrayTan([Tracked<Float>(1), Tracked<Float>(2)])))
  }

  do {
    // TF-952: Test array literal initialized from an address (e.g. `var`).
    func twoElementLiteralAddress(_ x: Tracked<Float>, _ y: Tracked<Float>) -> [Tracked<Float>] {
      var result = x
      result = result * y
      return [result, result]
    }
    let pb = pullback(at: 3, 4, in: twoElementLiteralAddress)
    expectEqual((8, 6), pb(TrackedFloatArrayTan([1, 1])))
  }

  do {
    // TF-952: Test array literal initialized with function call results.
    func twoElementLiteralFunctionResult(_ x: Tracked<Float>, _ y: Tracked<Float>) -> [Tracked<Float>] {
      return [x * y, x * y]
    }
    let pb = pullback(at: 3, 4, in: twoElementLiteralFunctionResult)
    expectEqual((8, 6), pb(TrackedFloatArrayTan([1, 1])))
  }

  do {
    // TF-975: Test multiple array literals.
    func twoElementLiterals(_ x: Tracked<Float>, _ y: Tracked<Float>) -> [Tracked<Float>] {
      let array = [x * y, x * y]
      return [array[0], array[1]]
    }
    let pb = pullback(at: 3, 4, in: twoElementLiterals)
    expectEqual((8, 6), pb(TrackedFloatArrayTan([1, 1])))
  }
}

ArrayAutoDiffTests.test("ArrayLiteralIndirect") {
  do {
    func twoElementLiteralIndirect<T: Differentiable>(_ x: T, _ y: T) -> [T] {
      return [x, y]
    }
    let pb = pullback(at: Float(1), 1, in: { twoElementLiteralIndirect($0, $1) })
    expectEqual((1, 2), pb(FloatArrayTan([1, 2])))
  }

  do {
    func twoElementLiteralIndirectVar<T: Differentiable>(_ x: T, _ y: T) -> [T] {
      var result: [T] = []
      result = result + [x]
      result = result + [y]
      return result
    }
    let pb = pullback(at: Float(1), 1, in: { twoElementLiteralIndirectVar($0, $1) })
    expectEqual((1, 2), pb(FloatArrayTan([1, 2])))
  }
}

struct Struct<T> {
  var x, y: T
}
extension Struct: Differentiable where T: Differentiable {}

ArrayAutoDiffTests.test("ArrayLiteralStruct") {
  typealias TV = Struct<Tracked<Float>>.TangentVector
  let s = Struct<Tracked<Float>>(x: 3, y: 4)

  do {
    func structElementLiteral<T>(_ s: Struct<T>) -> [T] {
      return [s.x, s.y]
    }
    func structGeneric<T>(_ s: Struct<T>) -> T {
      return structElementLiteral(s)[0]
    }
    func structConcrete1(_ s: Struct<Tracked<Float>>) -> Tracked<Float> {
      return structElementLiteral(s)[0] * structElementLiteral(s)[1]
    }
    func structConcrete2(_ s: Struct<Tracked<Float>>) -> Tracked<Float> {
      let array = structElementLiteral(s)
      return array[0] * array[1]
    }
    expectEqual(TV(x: 1, y: 0), gradient(at: s, in: { s in structGeneric(s) }))
    expectEqual(TV(x: 4, y: 3), gradient(at: s, in: structConcrete1))
    expectEqual(TV(x: 4, y: 3), gradient(at: s, in: structConcrete2))
  }

  do {
    func structElementAddressLiteral<T>(_ s: Struct<T>) -> [T] {
      var s2 = Struct<T>(x: s.x, y: s.y)
      return [s2.x, s2.y]
    }
    func structGeneric<T>(_ s: Struct<T>) -> T {
      return structElementAddressLiteral(s)[0]
    }
    func structConcrete1(_ s: Struct<Tracked<Float>>) -> Tracked<Float> {
      return structElementAddressLiteral(s)[0] *
             structElementAddressLiteral(s)[1]
    }
    func structConcrete2(_ s: Struct<Tracked<Float>>) -> Tracked<Float> {
      let array = structElementAddressLiteral(s)
      return array[0] * array[1]
    }
    expectEqual(TV(x: 1, y: 0), gradient(at: s, in: { s in structGeneric(s) }))
    expectEqual(TV(x: 4, y: 3), gradient(at: s, in: structConcrete1))
    expectEqual(TV(x: 4, y: 3), gradient(at: s, in: structConcrete2))
  }

  do {
    func structElementAddressLiteral2<T>(_ s: Struct<T>) -> [T] {
      var s2 = Struct<T>(x: s.x, y: s.y)
      let array = [s2.x, s2.y]
      return [array[0], array[1]]
    }
    func structGeneric<T>(_ s: Struct<T>) -> T {
      return structElementAddressLiteral2(s)[0]
    }
    func structConcrete1(_ s: Struct<Tracked<Float>>) -> Tracked<Float> {
      return structElementAddressLiteral2(s)[0] *
             structElementAddressLiteral2(s)[1]
    }
    func structConcrete2(_ s: Struct<Tracked<Float>>) -> Tracked<Float> {
      let array = structElementAddressLiteral2(s)
      return array[0] * array[1]
    }
    expectEqual(TV(x: 1, y: 0), gradient(at: s, in: { s in structGeneric(s) }))
    expectEqual(TV(x: 4, y: 3), gradient(at: s, in: structConcrete1))
    expectEqual(TV(x: 4, y: 3), gradient(at: s, in: structConcrete2))
  }

  // TF-978: Test array literal initialized with `apply` indirect results.

  do {
    func applyIndirectResult<T>(_ x: T, _ y: T) -> [Struct<T>] {
      return [Struct(x: x, y: y), Struct(x: x, y: y)]
    }
    let pb = pullback(at: Tracked<Float>(3), 4, in: { applyIndirectResult($0, $1) })
    let v = TV(x: 1, y: 1)
    expectEqual((2, 2), pb(.init([v, v])))
  }
}

ArrayAutoDiffTests.test("ArrayLiteralTuple") {
  do {
    func tupleElementGeneric<T>(_ x: T, _ y: T) -> [T] {
      var tuple = (x, y)
      return [tuple.0, tuple.1]
    }
    let pb = pullback(at: Tracked<Float>(3), 4, in: { tupleElementGeneric($0, $1) })
    // FIXME(TF-977): Fix incorrect derivative for array literal with
    // `tuple_element_addr` elements.
    // expectEqual((1, 1), pb(TrackedFloatArrayTan([1, 1])))
    expectEqual((0, 2), pb(TrackedFloatArrayTan([1, 1])))
  }
}

ArrayAutoDiffTests.testWithLeakChecking("ArrayLiteralNested") {
  do {
    func nested0(
        _ x: Tracked<Float>, _ y: Tracked<Float>, _ bool: Bool = true
    ) -> [Tracked<Float>] {
      let result = [[[[x, y]]]]
      return result[0][0][0]
    }
    let pb = pullback(at: 3, 4, in: { nested0($0, $1) })
    expectEqual((1, 1), pb(TrackedFloatArrayTan([1, 1, 1, 1])))
  }

  do {
    func nested1(
        _ x: Tracked<Float>, _ y: Tracked<Float>, _ bool: Bool = true
    ) -> [Tracked<Float>] {
      var result = [[x, y], [x, y]]
      return result[0] + result[1]
    }
    let pb = pullback(at: 3, 4, in: { nested1($0, $1) })
    expectEqual((2, 2), pb(TrackedFloatArrayTan([1, 1, 1, 1])))
  }

  do {
    // Convoluted function computing `[x + y]`.
    func nested2(
        _ x: Tracked<Float>, _ y: Tracked<Float>, _ bool: Bool = true
    ) -> [Tracked<Float>] {
      var result = [[], [x]]
      result = result + []
      result = result + [[]]
      result = result + [[y]]
      var nested = [result, [], result]
      return nested[0][1] + result[3]
    }
    let (value, pb) = valueWithPullback(at: 3, 4, in: { nested2($0, $1) })
    expectEqual([3, 4], value)
    expectEqual((1, 1), pb(TrackedFloatArrayTan([1, 1])))
  }
}

ArrayAutoDiffTests.testWithLeakChecking("ArrayLiteralControlFlow") {
  do {
    // TF-922: Test array literal and control flow.
    func controlFlow(
        _ x: Tracked<Float>, _ y: Tracked<Float>, _ bool: Bool = true
    ) -> [Tracked<Float>] {
      var result = [x * y, x * y]
      let result2 = bool ? result : result
      var result3 = bool ? (bool ? result2 : result) : result2
      return result3
    }
    let pb = pullback(at: 3, 4, in: { controlFlow($0, $1) })
    expectEqual((8, 6), pb(TrackedFloatArrayTan([1, 1])))
  }

  do {
    // TF-922: Test array literal and control flow.
    func controlFlowAddress(
        _ x: Tracked<Float>, _ y: Tracked<Float>, _ bool: Bool = true
    ) -> [Tracked<Float>] {
      var product = x * y // initial value is an address
      var result = [product, product]
      let result2 = bool ? result : result
      var result3 = bool ? (bool ? result2 : result) : result2
      return result3
    }
    let pb = pullback(at: 3, 4, in: { controlFlowAddress($0, $1) })
    expectEqual((8, 6), pb(TrackedFloatArrayTan([1, 1])))
  }

  do {
    // TF-922: Test array literal and control flow.
    func controlFlowGeneric<T>(_ x: T, _ y: T, _ bool: Bool = true) -> [T] {
      var result = [x, y] // initial values are addresses
      let result2 = bool ? result : result
      var result3 = bool ? (bool ? result2 : result) : result2
      return result3
    }
    let pb = pullback(at: Tracked<Float>(3), 4, in: { controlFlowGeneric($0, $1) })
    expectEqual((1, 1), pb(TrackedFloatArrayTan([1, 1])))
  }

  do {
    // Test nested array literal and control flow.
    func controlFlowNestedLiteral(
        _ x: Tracked<Float>, _ y: Tracked<Float>, _ bool: Bool = true
    ) -> [Tracked<Float>] {
      var result: [[Tracked<Float>]] = []
      var result2 = bool ? result + [[x]] : result + [[x]]
      var result3 = bool ? (bool ? result2 + [[y]] : result2 + [[y]]) : result2 + [[y]]
      return result3[0] + [result3[1][0]]
    }
    let pb = pullback(at: 3, 4, in: { controlFlowNestedLiteral($0, $1) })
    expectEqual((1, 1), pb(TrackedFloatArrayTan([1, 1])))
  }
}

ArrayAutoDiffTests.test("ExpressibleByArrayLiteralIndirect") {
  struct Indirect<T: Differentiable>: Differentiable & ExpressibleByArrayLiteral {
    var x: T

    typealias ArrayLiteralElement = T
    init(arrayLiteral: T...) {
      assert(arrayLiteral.count > 1)
      self.x = arrayLiteral[0]
    }
  }

  func testArrayUninitializedIntrinsic<T>(_ x: T, _ y: T) -> Indirect<T> {
    return [x, y]
  }

  let (gradX, gradY) = pullback(at: Float(1), Float(1), in: {
    x, y in testArrayUninitializedIntrinsic(x, y)
  })(Indirect<Float>.TangentVector(x: 1))
  expectEqual(1, gradX)
  expectEqual(0, gradY)
}

ArrayAutoDiffTests.test("Array.+") {
  struct TwoArrays : Differentiable {
    var a: [Float]
    var b: [Float]
  }

  func sumFirstThreeConcatenated(_ arrs: TwoArrays) -> Float {
    let c = arrs.a + arrs.b
    return c[0] + c[1] + c[2]
  }

  expectEqual(
    TwoArrays.TangentVector(
      a: FloatArrayTan([1, 1]),
      b: FloatArrayTan([1, 0])),
    gradient(
      at: TwoArrays(a: [0, 0], b: [0, 0]),
      in: sumFirstThreeConcatenated))
  expectEqual(
    TwoArrays.TangentVector(
      a: FloatArrayTan([1, 1, 1, 0]),
      b: FloatArrayTan([0, 0])),
    gradient(
      at: TwoArrays(a: [0, 0, 0, 0], b: [0, 0]),
      in: sumFirstThreeConcatenated))
  expectEqual(
    TwoArrays.TangentVector(
      a: FloatArrayTan([]),
      b: FloatArrayTan([1, 1, 1, 0])),
    gradient(
      at: TwoArrays(a: [], b: [0, 0, 0, 0]),
      in: sumFirstThreeConcatenated))

  func identity(_ array: [Tracked<Float>]) -> [Tracked<Float>] {
    var results: [Tracked<Float>] = []
    for i in withoutDerivative(at: array.indices) {
      results = results + [array[i]]
    }
    return results
  }
  let v = TrackedFloatArrayTan([4, -5, 6])
  expectEqual(v, pullback(at: [1, 2, 3], in: identity)(v))
}

ArrayAutoDiffTests.test("Array.append") {
  func identity(_ array: [Tracked<Float>]) -> [Tracked<Float>] {
    var results: [Tracked<Float>] = []
    for i in withoutDerivative(at: array.indices) {
      results.append(array[i])
    }
    return results
  }
  let v = TrackedFloatArrayTan([4, -5, 6])
  expectEqual(v, pullback(at: [1, 2, 3], in: identity)(v))
}

ArrayAutoDiffTests.testWithLeakChecking("Array.init(repeating:count:)") {
  @differentiable
  func repeating(_ x: Tracked<Float>) -> [Tracked<Float>] {
    Array(repeating: x, count: 10)
  }
  expectEqual(Tracked<Float>(10), gradient(at: .zero) { x in
    repeating(x).differentiableReduce(0, {$0 + $1}).value
  })
  expectEqual(Tracked<Float>(20), pullback(at: .zero, in: { x in
    repeating(x).differentiableReduce(0, {$0 + $1}).value
  })(2))
}

ArrayAutoDiffTests.test("Array.DifferentiableView.init") {
  @differentiable
  func constructView(_ x: [Float]) -> Array<Float>.DifferentiableView {
    return Array<Float>.DifferentiableView(x)
  }

  let backprop = pullback(at: [5, 6, 7, 8], in: constructView)
  expectEqual(
    FloatArrayTan([1, 2, 3, 4]),
    backprop(FloatArrayTan([1, 2, 3, 4])))
}

ArrayAutoDiffTests.test("Array.DifferentiableView.base") {
  @differentiable
  func accessBase(_ x: Array<Float>.DifferentiableView) -> [Float] {
    return x.base
  }

  let backprop = pullback(
    at: Array<Float>.DifferentiableView([5, 6, 7, 8]),
    in: accessBase)
  expectEqual(
    FloatArrayTan([1, 2, 3, 4]),
    backprop(FloatArrayTan([1, 2, 3, 4])))
}

ArrayAutoDiffTests.test("Array.DifferentiableView : KeyPathIterable") {
  struct Container : KeyPathIterable {
    let a: Array<Float>.DifferentiableView
  }
  let container = Container(a: Array<Float>.DifferentiableView([1, 2, 3]))
  expectEqual(
    [1, 2, 3],
    container.recursivelyAllKeyPaths(to: Float.self).map {
      container[keyPath: $0]
    })
}

ArrayAutoDiffTests.test("Array.zeroTangentVectorInitializer") {
  let count = 10
  let array: [Float] = Array((0..<count).map(Float.init))
  expectEqual(array.zeroTangentVector.base, Array(repeating: 0, count: count))
}

runAllTests()
