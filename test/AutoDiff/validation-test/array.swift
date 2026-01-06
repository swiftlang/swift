// RUN: %target-run-simple-swift

// REQUIRES: executable_test

// Would fail due to unavailability of swift_autoDiffCreateLinearMapContext.

import StdlibUnittest
import _Differentiation

var ArrayAutoDiffTests = TestSuite("ArrayAutoDiff")

typealias FloatArrayTan = Array<Float>.TangentVector

extension Array.DifferentiableView {
  /// A subscript that always fatal errors.
  ///
  /// The differentiation transform should never emit calls to this.
  subscript(alwaysFatalError: Int) -> Element {
    fatalError("wrong subscript")
  }
}

ArrayAutoDiffTests.test("ArrayIdentity") {
  func arrayIdentity(_ x: [Float]) -> [Float] {
    return x
  }

  let backprop = pullback(at: [5, 6, 7, 8], of: arrayIdentity)
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
    gradient(at: [2, 3, 4, 5, 6, 7], of: sumFirstThree))
}

ArrayAutoDiffTests.test("ArrayLiteral") {
  do {
    func twoElementLiteral(_ x: Float, _ y: Float) -> [Float] {
      return [x, y]
    }
    let pb = pullback(at: 1, 1, of: twoElementLiteral)
    expectEqual((1, 2), pb(FloatArrayTan([Float(1), Float(2)])))
  }

  do {
    // TF-952: Test array literal initialized from an address (e.g. `var`).
    func twoElementLiteralAddress(_ x: Float, _ y: Float) -> [Float] {
      var result = x
      result = result * y
      return [result, result]
    }
    let pb = pullback(at: 3, 4, of: twoElementLiteralAddress)
    expectEqual((8, 6), pb(FloatArrayTan([1, 1])))
  }

  do {
    // TF-952: Test array literal initialized with function call results.
    func twoElementLiteralFunctionResult(_ x: Float, _ y: Float) -> [Float] {
      return [x * y, x * y]
    }
    let pb = pullback(at: 3, 4, of: twoElementLiteralFunctionResult)
    expectEqual((8, 6), pb(FloatArrayTan([1, 1])))
  }

  do {
    // TF-975: Test multiple array literals.
    func twoElementLiterals(_ x: Float, _ y: Float) -> [Float] {
      let array = [x * y, x * y]
      return [array[0], array[1]]
    }
    let pb = pullback(at: 3, 4, of: twoElementLiterals)
    expectEqual((8, 6), pb(FloatArrayTan([1, 1])))
  }
}

ArrayAutoDiffTests.test("ArrayLiteralIndirect") {
  do {
    func twoElementLiteralIndirect<T: Differentiable>(_ x: T, _ y: T) -> [T] {
      return [x, y]
    }
    let pb = pullback(at: Float(1), 1, of: { twoElementLiteralIndirect($0, $1) })
    expectEqual((1, 2), pb(FloatArrayTan([1, 2])))
  }

  do {
    func twoElementLiteralIndirectVar<T: Differentiable>(_ x: T, _ y: T) -> [T] {
      var result: [T] = []
      result = result + [x]
      result = result + [y]
      return result
    }
    let pb = pullback(at: Float(1), 1, of: { twoElementLiteralIndirectVar($0, $1) })
    expectEqual((1, 2), pb(FloatArrayTan([1, 2])))
  }
}

struct Struct<T> {
  var x, y: T
}
extension Struct: Differentiable where T: Differentiable {}

ArrayAutoDiffTests.test("ArrayLiteralStruct") {
  typealias TV = Struct<Float>.TangentVector
  let s = Struct<Float>(x: 3, y: 4)

  do {
    func structElementLiteral<T>(_ s: Struct<T>) -> [T] {
      return [s.x, s.y]
    }
    func structGeneric<T>(_ s: Struct<T>) -> T {
      return structElementLiteral(s)[0]
    }
    func structConcrete1(_ s: Struct<Float>) -> Float {
      return structElementLiteral(s)[0] * structElementLiteral(s)[1]
    }
    func structConcrete2(_ s: Struct<Float>) -> Float {
      let array = structElementLiteral(s)
      return array[0] * array[1]
    }
    expectEqual(TV(x: 1, y: 0), gradient(at: s, of: { s in structGeneric(s) }))
    expectEqual(TV(x: 4, y: 3), gradient(at: s, of: structConcrete1))
    expectEqual(TV(x: 4, y: 3), gradient(at: s, of: structConcrete2))
  }

  do {
    func structElementAddressLiteral<T>(_ s: Struct<T>) -> [T] {
      var s2 = Struct<T>(x: s.x, y: s.y)
      return [s2.x, s2.y]
    }
    func structGeneric<T>(_ s: Struct<T>) -> T {
      return structElementAddressLiteral(s)[0]
    }
    func structConcrete1(_ s: Struct<Float>) -> Float {
      return structElementAddressLiteral(s)[0] *
             structElementAddressLiteral(s)[1]
    }
    func structConcrete2(_ s: Struct<Float>) -> Float {
      let array = structElementAddressLiteral(s)
      return array[0] * array[1]
    }
    expectEqual(TV(x: 1, y: 0), gradient(at: s, of: { s in structGeneric(s) }))
    expectEqual(TV(x: 4, y: 3), gradient(at: s, of: structConcrete1))
    expectEqual(TV(x: 4, y: 3), gradient(at: s, of: structConcrete2))
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
    func structConcrete1(_ s: Struct<Float>) -> Float {
      return structElementAddressLiteral2(s)[0] *
             structElementAddressLiteral2(s)[1]
    }
    func structConcrete2(_ s: Struct<Float>) -> Float {
      let array = structElementAddressLiteral2(s)
      return array[0] * array[1]
    }
    expectEqual(TV(x: 1, y: 0), gradient(at: s, of: { s in structGeneric(s) }))
    expectEqual(TV(x: 4, y: 3), gradient(at: s, of: structConcrete1))
    expectEqual(TV(x: 4, y: 3), gradient(at: s, of: structConcrete2))
  }

  // TF-978: Test array literal initialized with `apply` indirect results.

  do {
    func applyIndirectResult<T>(_ x: T, _ y: T) -> [Struct<T>] {
      return [Struct(x: x, y: y), Struct(x: x, y: y)]
    }
    let pb = pullback(at: Float(3), 4, of: { applyIndirectResult($0, $1) })
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
    let pb = pullback(at: Float(3), 4, of: { tupleElementGeneric($0, $1) })
    expectEqual((1, 1), pb(FloatArrayTan([1, 1])))
  }
}

ArrayAutoDiffTests.test("ArrayLiteralNested") {
  do {
    func nested0(
        _ x: Float, _ y: Float, _ bool: Bool = true
    ) -> [Float] {
      let result = [[[[x, y]]]]
      return result[0][0][0]
    }
    let pb = pullback(at: 3, 4, of: { nested0($0, $1) })
    expectEqual((1, 1), pb(FloatArrayTan([1, 1, 1, 1])))
  }

  do {
    func nested1(
        _ x: Float, _ y: Float, _ bool: Bool = true
    ) -> [Float] {
      var result = [[x, y], [x, y]]
      return result[0] + result[1]
    }
    let pb = pullback(at: 3, 4, of: { nested1($0, $1) })
    expectEqual((2, 2), pb(FloatArrayTan([1, 1, 1, 1])))
  }

  do {
    // Convoluted function computing `[x + y]`.
    func nested2(
        _ x: Float, _ y: Float, _ bool: Bool = true
    ) -> [Float] {
      var result = [[], [x]]
      result = result + []
      result = result + [[]]
      result = result + [[y]]
      var nested = [result, [], result]
      return nested[0][1] + result[3]
    }
    let (value, pb) = valueWithPullback(at: 3, 4, of: { nested2($0, $1) })
    expectEqual([3, 4], value)
    expectEqual((1, 1), pb(FloatArrayTan([1, 1])))
  }
}

ArrayAutoDiffTests.test("ArrayLiteralControlFlow") {
  do {
    // TF-922: Test array literal and control flow.
    func controlFlow(
        _ x: Float, _ y: Float, _ bool: Bool = true
    ) -> [Float] {
      var result = [x * y, x * y]
      let result2 = bool ? result : result
      var result3 = bool ? (bool ? result2 : result) : result2
      return result3
    }
    let pb = pullback(at: 3, 4, of: { controlFlow($0, $1) })
    expectEqual((8, 6), pb(FloatArrayTan([1, 1])))
  }

  do {
    // TF-922: Test array literal and control flow.
    func controlFlowAddress(
        _ x: Float, _ y: Float, _ bool: Bool = true
    ) -> [Float] {
      var product = x * y // initial value is an address
      var result = [product, product]
      let result2 = bool ? result : result
      var result3 = bool ? (bool ? result2 : result) : result2
      return result3
    }
    let pb = pullback(at: 3, 4, of: { controlFlowAddress($0, $1) })
    expectEqual((8, 6), pb(FloatArrayTan([1, 1])))
  }

  do {
    // TF-922: Test array literal and control flow.
    func controlFlowGeneric<T>(_ x: T, _ y: T, _ bool: Bool = true) -> [T] {
      var result = [x, y] // initial values are addresses
      let result2 = bool ? result : result
      var result3 = bool ? (bool ? result2 : result) : result2
      return result3
    }
    let pb = pullback(at: Float(3), 4, of: { controlFlowGeneric($0, $1) })
    expectEqual((1, 1), pb(FloatArrayTan([1, 1])))
  }

  do {
    // Test nested array literal and control flow.
    func controlFlowNestedLiteral(
        _ x: Float, _ y: Float, _ bool: Bool = true
    ) -> [Float] {
      var result: [[Float]] = []
      var result2 = bool ? result + [[x]] : result + [[x]]
      var result3 = bool ? (bool ? result2 + [[y]] : result2 + [[y]]) : result2 + [[y]]
      return result3[0] + [result3[1][0]]
    }
    let pb = pullback(at: 3, 4, of: { controlFlowNestedLiteral($0, $1) })
    expectEqual((1, 1), pb(FloatArrayTan([1, 1])))
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

  let (gradX, gradY) = pullback(at: Float(1), Float(1), of: {
    x, y in testArrayUninitializedIntrinsic(x, y)
  })(Indirect<Float>.TangentVector(x: 1))
  expectEqual(1, gradX)
  expectEqual(0, gradY)
}

ArrayAutoDiffTests.test("Array.+") {
  func sumFirstThreeConcatenating(_ a: [Float], _ b: [Float]) -> Float {
    let c = a + b
    return c[0] + c[1] + c[2]
  }

  expectEqual(
    (.init([1, 1]), .init([1, 0])),
    gradient(at: [0, 0], [0, 0], of: sumFirstThreeConcatenating))
  expectEqual(
    (.init([1, 1, 1, 0]), .init([0, 0])),
    gradient(at: [0, 0, 0, 0], [0, 0], of: sumFirstThreeConcatenating))
  expectEqual(
    (.init([]), .init([1, 1, 1, 0])),
    gradient(at: [], [0, 0, 0, 0], of: sumFirstThreeConcatenating))

  func identity(_ array: [Float]) -> [Float] {
    var results: [Float] = []
    for i in withoutDerivative(at: array.indices) {
      results = results + [array[i]]
    }
    return results
  }
  let v = FloatArrayTan([4, -5, 6])
  expectEqual(v, pullback(at: [1, 2, 3], of: identity)(v))
  
  let v1: [Float] = [1, 1]
  let v2: [Float] = [1, 1, 1]
  expectEqual((.zero, .zero), pullback(at: v1, v2, of: +)(.zero))
}

ArrayAutoDiffTests.test("Array.+=") {
  func sumFirstThreeConcatenating(_ a: [Float], _ b: [Float]) -> Float {
    var c = a
    c += b
    return c[0] + c[1] + c[2]
  }

  expectEqual(
    (.init([1, 1]), .init([1, 0])),
    gradient(at: [0, 0], [0, 0], of: sumFirstThreeConcatenating))
  expectEqual(
    (.init([1, 1, 1, 0]), .init([0, 0])),
    gradient(at: [0, 0, 0, 0], [0, 0], of: sumFirstThreeConcatenating))
  expectEqual(
    (.init([]), .init([1, 1, 1, 0])),
    gradient(at: [], [0, 0, 0, 0], of: sumFirstThreeConcatenating))

  func identity(_ array: [Float]) -> [Float] {
    var results: [Float] = []
    for i in withoutDerivative(at: array.indices) {
      results += [array[i]]
    }
    return results
  }
  let v = FloatArrayTan([4, -5, 6])
  expectEqual(v, pullback(at: [1, 2, 3], of: identity)(v))
}

ArrayAutoDiffTests.test("Array.append") {
  func appending(_ array: [Float], _ element: Float) -> [Float] {
    var result = array
    result.append(element)
    return result
  }
  do {
    let v = FloatArrayTan([1, 2, 3, 4])
    expectEqual((.init([1, 2, 3]), 4),
                pullback(at: [0, 0, 0], 0, of: appending)(v))
  }

  func identity(_ array: [Float]) -> [Float] {
    var results: [Float] = []
    for i in withoutDerivative(at: array.indices) {
      results.append(array[i])
    }
    return results
  }
  do {
    let v = FloatArrayTan([4, -5, 6])
    expectEqual(v, pullback(at: [1, 2, 3], of: identity)(v))
  }
}

ArrayAutoDiffTests.test("Array.init(repeating:count:)") {
  @differentiable(reverse)
  func repeating(_ x: Float) -> [Float] {
    Array(repeating: x, count: 10)
  }
  expectEqual(Float(10), gradient(at: .zero) { x in
    repeating(x).differentiableReduce(0, {$0 + $1})
  })
  expectEqual(Float(20), pullback(at: .zero, of: { x in
    repeating(x).differentiableReduce(0, {$0 + $1})
  })(2))
}

ArrayAutoDiffTests.test("Array.DifferentiableView.init") {
  @differentiable(reverse)
  func constructView(_ x: [Float]) -> Array<Float>.DifferentiableView {
    return Array<Float>.DifferentiableView(x)
  }

  let backprop = pullback(at: [5, 6, 7, 8], of: constructView)
  expectEqual(
    FloatArrayTan([1, 2, 3, 4]),
    backprop(FloatArrayTan([1, 2, 3, 4])))
}

ArrayAutoDiffTests.test("Array.DifferentiableView.base") {
  @differentiable(reverse)
  func accessBase(_ x: Array<Float>.DifferentiableView) -> [Float] {
    return x.base
  }

  let backprop = pullback(
    at: Array<Float>.DifferentiableView([5, 6, 7, 8]),
    of: accessBase)
  expectEqual(
    FloatArrayTan([1, 2, 3, 4]),
    backprop(FloatArrayTan([1, 2, 3, 4])))
}

ArrayAutoDiffTests.test("Array.DifferentiableView.move") {
  var v: [Float] = [1, 2, 3]
  v.move(by: .zero)
  expectEqual(v, [1, 2, 3])

  var z: [Float] = []
  z.move(by: .zero)
  expectEqual(z, [])
}

ArrayAutoDiffTests.test("Array.DifferentiableView reflection") {
  let tan = [Float].DifferentiableView([41, 42])
  let children = Array(Mirror(reflecting: tan).children)
  expectEqual(2, children.count)
  if let child1 = expectNotNil(children[0].value as? Float),
     let child2 = expectNotNil(children[1].value as? Float) {
    expectEqual(41, child1)
    expectEqual(42, child2)
  }
}

runAllTests()
