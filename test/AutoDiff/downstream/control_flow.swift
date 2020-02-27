// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

var ControlFlowTests = TestSuite("ControlFlow")

ControlFlowTests.test("Conditionals") {
  func cond1(_ x: Float) -> Float {
    if x > 0 {
      return x * x
    }
    return x + x
  }
  expectEqual(8, gradient(at: 4, in: cond1))
  expectEqual(2, gradient(at: -10, in: cond1))

  func cond2(_ x: Float) -> Float {
    let y: Float
    if x > 0 {
      y = x * x
    } else if x == -1337 {
      y = 0
    } else {
      y = x + x
    }
    return y
  }
  expectEqual(8, gradient(at: 4, in: cond2))
  expectEqual(2, gradient(at: -10, in: cond2))
  expectEqual(0, gradient(at: -1337, in: cond2))

  func cond2_var(_ x: Float) -> Float {
    var y: Float = x
    if x > 0 {
      y = y * x
    } else if x == -1337 {
      y = x // Dummy assignment; shouldn't affect computation.
      y = x // Dummy assignment; shouldn't affect computation.
      y = 0
    } else {
      y = x + y
    }
    return y
  }
  expectEqual(8, gradient(at: 4, in: cond2_var))
  expectEqual(2, gradient(at: -10, in: cond2_var))
  expectEqual(0, gradient(at: -1337, in: cond2_var))

  func cond3(_ x: Float, _ y: Float) -> Float {
    if x > 0 {
      return x * y
    }
    return y - x
  }
  expectEqual((5, 4), gradient(at: 4, 5, in: cond3))
  expectEqual((-1, 1), gradient(at: -3, -2, in: cond3))

  func cond4_var(_ x: Float) -> Float {
    var outer = x
    outerIf: if true {
      var inner = outer
      inner = inner * x
      if false {
        break outerIf
      }
      outer = inner
    }
    return outer
  }
  expectEqual((9, 6), valueWithGradient(at: 3, in: cond4_var))

  func cond_tuple(_ x: Float) -> Float {
    // Convoluted function returning `x + x`.
    let y: (Float, Float) = (x, x)
    if x > 0 {
      return y.0 + y.1
    }
    return y.0 + y.0 - y.1 + y.0
  }
  expectEqual((8, 2), valueWithGradient(at: 4, in: cond_tuple))
  expectEqual((-20, 2), valueWithGradient(at: -10, in: cond_tuple))
  expectEqual((-2674, 2), valueWithGradient(at: -1337, in: cond_tuple))

  func cond_tuple2(_ x: Float) -> Float {
    // Convoluted function returning `x + x`.
    let y: (Float, Float) = (x, x)
    let y0 = y.0
    if x > 0 {
      let y1 = y.1
      return y0 + y1
    }
    let y0_double = y0 + y.0
    let y1 = y.1
    return y0_double - y1 + y.0
  }
  expectEqual((8, 2), valueWithGradient(at: 4, in: cond_tuple2))
  expectEqual((-20, 2), valueWithGradient(at: -10, in: cond_tuple2))
  expectEqual((-2674, 2), valueWithGradient(at: -1337, in: cond_tuple2))

  func cond_tuple_var(_ x: Float) -> Float {
    // Convoluted function returning `x + x`.
    var y: (Float, Float) = (x, x)
    var z: (Float, Float) = (x + x, x - x)
    if x > 0 {
      var w = (x, x)
      y.0 = w.1
      y.1 = w.0
      z.0 = z.0 - y.0
      z.1 = z.1 + y.0
    } else {
      z = (x, x)
    }
    return y.0 + y.1 - z.0 + z.1
  }
  expectEqual((8, 2), valueWithGradient(at: 4, in: cond_tuple_var))
  expectEqual((-20, 2), valueWithGradient(at: -10, in: cond_tuple_var))
  expectEqual((-2674, 2), valueWithGradient(at: -1337, in: cond_tuple_var))

  func cond_nestedtuple_var(_ x: Float) -> Float {
    // Convoluted function returning `x + x`.
    var y: (Float, Float) = (x + x, x - x)
    var z: ((Float, Float), Float) = (y, x)
    if x > 0 {
      var w = (x, x)
      y.0 = w.1
      y.1 = w.0
      z.0.0 = z.0.0 - y.0
      z.0.1 = z.0.1 + y.0
    } else {
      z = ((y.0 - x, y.1 + x), x)
    }
    return y.0 + y.1 - z.0.0 + z.0.1
  }
  expectEqual((8, 2), valueWithGradient(at: 4, in: cond_nestedtuple_var))
  expectEqual((-20, 2), valueWithGradient(at: -10, in: cond_nestedtuple_var))
  expectEqual((-2674, 2), valueWithGradient(at: -1337, in: cond_nestedtuple_var))

  struct FloatPair : Differentiable {
    var first, second: Float
    init(_ first: Float, _ second: Float) {
      self.first = first
      self.second = second
    }
  }

  struct Pair<T : Differentiable, U : Differentiable> : Differentiable {
    var first: T
    var second: U
    init(_ first: T, _ second: U) {
      self.first = first
      self.second = second
    }
  }

  func cond_struct(_ x: Float) -> Float {
    // Convoluted function returning `x + x`.
    let y = FloatPair(x, x)
    if x > 0 {
      return y.first + y.second
    }
    return y.first + y.first - y.second + y.first
  }
  expectEqual((8, 2), valueWithGradient(at: 4, in: cond_struct))
  expectEqual((-20, 2), valueWithGradient(at: -10, in: cond_struct))
  expectEqual((-2674, 2), valueWithGradient(at: -1337, in: cond_struct))

  func cond_struct2(_ x: Float) -> Float {
    // Convoluted function returning `x + x`.
    let y = FloatPair(x, x)
    let y0 = y.first
    if x > 0 {
      let y1 = y.second
      return y0 + y1
    }
    let y0_double = y0 + y.first
    let y1 = y.second
    return y0_double - y1 + y.first
  }
  expectEqual((8, 2), valueWithGradient(at: 4, in: cond_struct2))
  expectEqual((-20, 2), valueWithGradient(at: -10, in: cond_struct2))
  expectEqual((-2674, 2), valueWithGradient(at: -1337, in: cond_struct2))

  func cond_struct_var(_ x: Float) -> Float {
    // Convoluted function returning `x + x`.
    var y = FloatPair(x, x)
    var z = FloatPair(x + x, x - x)
    if x > 0 {
      var w = y
      y.first = w.second
      y.second = w.first
      z.first = z.first - y.first
      z.second = z.second + y.first
    } else {
      z = FloatPair(x, x)
    }
    return y.first + y.second - z.first + z.second
  }
  expectEqual((8, 2), valueWithGradient(at: 4, in: cond_struct_var))
  expectEqual((-20, 2), valueWithGradient(at: -10, in: cond_struct_var))
  expectEqual((-2674, 2), valueWithGradient(at: -1337, in: cond_struct_var))

  func cond_nestedstruct_var(_ x: Float) -> Float {
    // Convoluted function returning `x + x`.
    var y = FloatPair(x + x, x - x)
    var z = Pair(y, x)
    if x > 0 {
      var w = FloatPair(x, x)
      y.first = w.second
      y.second = w.first
      z.first.first = z.first.first - y.first
      z.first.second = z.first.second + y.first
    } else {
      z = Pair(FloatPair(y.first - x, y.second + x), x)
    }
    return y.first + y.second - z.first.first + z.first.second
  }
  expectEqual((8, 2), valueWithGradient(at: 4, in: cond_nestedstruct_var))
  expectEqual((-20, 2), valueWithGradient(at: -10, in: cond_nestedstruct_var))
  expectEqual((-2674, 2), valueWithGradient(at: -1337, in: cond_nestedstruct_var))

  func guard1(_ x: Float, _ y: Float) -> Float {
    guard x > 0 else {
      return x * x
    }
    return y * y
  }
  expectEqual((0, 10), gradient(at: 4, 5, in: guard1))
  expectEqual((-6, 0), gradient(at: -3, -2, in: guard1))

  func guard2(_ x: Float, _ y: Float) -> Float {
    guard x > 0 else {
      if y > 0 {
        return x * y
      } else if x == -1337 {
        return x * x
      }
      return 0
    }
    return y * y
  }
  expectEqual((0, 10), gradient(at: 4, 5, in: guard2))
  expectEqual((5, -1337), gradient(at: -1337, 5, in: guard2))
  expectEqual((-2674, 0), gradient(at: -1337, -5, in: guard2))
  expectEqual((2, -3), gradient(at: -3, 2, in: guard2))

  func guard2_var(_ x: Float, _ y: Float) -> Float {
    var z = y
    guard x > 0 else {
      if y > 0 {
        z = z * x
      } else if x == -1337 {
        z = x
        z = z * z
      } else {
        z = 0
      }
      return z
    }
    return z * y
  }
  expectEqual((0, 10), gradient(at: 4, 5, in: guard2_var))
  expectEqual((5, -1337), gradient(at: -1337, 5, in: guard2_var))
  expectEqual((-2674, 0), gradient(at: -1337, -5, in: guard2_var))
  expectEqual((2, -3), gradient(at: -3, 2, in: guard2_var))

  func guard3(_ x: Float, _ y: Float) -> Float {
    guard x > 0 else {
      fatalError()
    }
    return y * y
  }
  expectEqual((0, 10), gradient(at: 4, 5, in: guard3))
  expectCrash {
    _ = gradient(at: -3, -2, in: guard3)
  }

  func cond_empty(_ x: Float) -> Float {
    if x > 0 {
      // Create empty trampoline blocks.
    }
    return x * x
  }
  expectEqual(4, gradient(at: 2, in: cond_empty))
  expectEqual(-6, gradient(at: -3, in: cond_empty))

  func cond_generic<T : Differentiable & FloatingPoint>(
    _ x: T, _ y: T
  ) -> T {
    if x > 0 {
      return x
    }
    return y
  }
  expectEqual((1, 0), gradient(at: 4, 5, in: { x, y in cond_generic(x, y) }))
  expectEqual((0, 1), gradient(at: -4, 5, in: { x, y in cond_generic(x, y) }))
}

ControlFlowTests.test("NestedConditionals") {
  func nested1(_ x: Float) -> Float {
    if x > 0 {
      if x > 10 {
        return x * x + x
      } else {
        return x * x
      }
    }
    return x * -x
  }
  expectEqual(23, gradient(at: 11, in: nested1))
  expectEqual(8, gradient(at: 4, in: nested1))
  expectEqual(20, gradient(at: -10, in: nested1))

  func nested2(_ x: Float, _ y: Float) -> Float {
    if x > 0 {
      if y > 10 {
        return x * y
      } else {
        return x + y
      }
    }
    return -y
  }
  expectEqual((20, 4), gradient(at: 4, 20, in: nested2))
  expectEqual((1, 1), gradient(at: 4, 5, in: nested2))
  expectEqual((0, -1), gradient(at: -3, -2, in: nested2))

  func nested3(_ x: Float, _ y: Float) -> Float {
    if x > 0 {
      if y > 10 {
        let z = x * y
        if z > 100 {
          return x + z
        } else if y == 20 {
          return z + z
        }
      } else {
        return x + y
      }
    }
    return -y
  }
  expectEqual((40, 8), gradient(at: 4, 20, in: nested3))
  expectEqual((0, -1), gradient(at: 4, 21, in: nested3))
  expectEqual((1, 1), gradient(at: 4, 5, in: nested3))
  expectEqual((0, -1), gradient(at: -3, -2, in: nested3))

  func nested3_var(_ x: Float, _ y: Float) -> Float {
    var w = y
    if x > 0 {
      if y > 10 {
        var z = x * w
        if z > 100 {
          z = x + z
          return z
        } else if y == 20 {
          z = z + z
          return z
        }
      } else {
        w = x + w
        return w
      }
    }
    w = -w
    return w
  }
  expectEqual((40, 8), gradient(at: 4, 20, in: nested3))
  expectEqual((0, -1), gradient(at: 4, 21, in: nested3))
  expectEqual((1, 1), gradient(at: 4, 5, in: nested3))
  expectEqual((0, -1), gradient(at: -3, -2, in: nested3))

  // TF-781: nested if derivative correctness.
  do {
    struct TF_781: Differentiable {
      var w: Float = 3

      @differentiable(wrt: self) // wrt only self is important
      func callAsFunction(_ input: Float) -> Float {
        var x = input
        if true {
          if true {
            // Function application below should make `self` have non-zero
            // derivative.
            x = x * w
          }
        }
        return x
      }
    }
    let x: Float = 10
    expectEqual(TF_781.TangentVector(w: x), gradient(at: TF_781()) { $0(x) })
  }

  // Non-method version of TF-781.
  do {
    @differentiable(wrt: x)
    func TF_781(_ x: Float, _ y: Float) -> Float {
      var result = y
      if true {
        if true {
          result = result * x
        }
      }
      return result
    }
    let x: Float = 10
    expectEqual(x, gradient(at: 3) { TF_781($0, x) })
  }
}

ControlFlowTests.test("Recursion") {
  func factorial(_ x: Float) -> Float {
    if x == 1 {
      return 1
    }
    return x * factorial(x - 1)
  }
  expectEqual(0, gradient(at: 1, in: factorial))
  expectEqual(1, gradient(at: 2, in: factorial))
  expectEqual(5, gradient(at: 3, in: factorial))
  expectEqual(26, gradient(at: 4, in: factorial))
  expectEqual(154, gradient(at: 5, in: factorial))

  func factorial_var1(_ x: Float) -> Float {
    var y: Float = x
    if x == 1 {
      y = 1
    } else {
      y = x
      y = y * factorial_var1(y - 1)
    }
    return y
  }
  expectEqual(0, gradient(at: 1, in: factorial_var1))
  expectEqual(1, gradient(at: 2, in: factorial_var1))
  expectEqual(5, gradient(at: 3, in: factorial_var1))
  expectEqual(26, gradient(at: 4, in: factorial_var1))
  expectEqual(154, gradient(at: 5, in: factorial_var1))

  func factorial_var2(_ x: Float) -> Float {
    // Next line is the only difference with `factorial_var1`.
    var y: Float = 1
    if x == 1 {
      y = 1
    } else {
      y = x
      y = y * factorial_var2(y - 1)
    }
    return y
  }
  expectEqual(0, gradient(at: 1, in: factorial_var2))
  expectEqual(1, gradient(at: 2, in: factorial_var2))
  expectEqual(5, gradient(at: 3, in: factorial_var2))
  expectEqual(26, gradient(at: 4, in: factorial_var2))
  expectEqual(154, gradient(at: 5, in: factorial_var2))

  func product(_ x: Float, count: Int) -> Float {
    precondition(count > 0)
    if count == 1 {
      return x
    }
    return x * product(x, count: count - 1)
  }
  expectEqual(300, gradient(at: 10, in: { x in product(x, count: 3) }))
  expectEqual(-20, gradient(at: -10, in: { x in product(x, count: 2) }))
  expectEqual(1, gradient(at: 100, in: { x in product(x, count: 1) }))
}

ControlFlowTests.test("Enums") {
  enum Enum {
    case a(Float)
    case b(Float, Float)

    func enum_notactive1(_ x: Float) -> Float {
      switch self {
      case let .a(a): return x * a
      case let .b(b1, b2): return x * b1 * b2
      }
    }
  }

  func enum_notactive1(_ e: Enum, _ x: Float) -> Float {
    switch e {
    case let .a(a): return x * a
    case let .b(b1, b2): return x * b1 * b2
    }
  }
  expectEqual(10, gradient(at: 2, in: { x in enum_notactive1(.a(10), x) }))
  expectEqual(10, gradient(at: 2, in: { x in Enum.a(10).enum_notactive1(x) }))
  expectEqual(20, gradient(at: 2, in: { x in enum_notactive1(.b(4, 5), x) }))
  expectEqual(20, gradient(at: 2, in: { x in Enum.b(4, 5).enum_notactive1(x) }))

  func enum_notactive2(_ e: Enum, _ x: Float) -> Float {
    var y = x
    if x > 0 {
      var z = y + y
      switch e {
      case .a: z = z - y
      case .b: y = y + x
      }
      var w = y
      if case .a = e {
        w = w + z
      }
      return w
    } else if case .b = e {
      return y + y
    }
    return x + y
  }
  expectEqual((8, 2), valueWithGradient(at: 4, in: { x in enum_notactive2(.a(10), x) }))
  expectEqual((20, 2), valueWithGradient(at: 10, in: { x in enum_notactive2(.b(4, 5), x) }))
  expectEqual((-20, 2), valueWithGradient(at: -10, in: { x in enum_notactive2(.a(10), x) }))
  expectEqual((-2674, 2), valueWithGradient(at: -1337, in: { x in enum_notactive2(.b(4, 5), x) }))

  func optional_notactive1(_ optional: Float?, _ x: Float) -> Float {
    if let y = optional {
      return x * y
    }
    return x + x
  }
  expectEqual(2, gradient(at: 2, in: { x in optional_notactive1(nil, x) }))
  expectEqual(10, gradient(at: 2, in: { x in optional_notactive1(10, x) }))

  struct Dense : Differentiable {
    var w1: Float
    @noDerivative var w2: Float?

    @differentiable
    func callAsFunction(_ input: Float) -> Float {
      if let w2 = w2 {
        return input * w1 * w2
      }
      return input * w1
    }
  }
  expectEqual((Dense.TangentVector(w1: 10), 20),
              gradient(at: Dense(w1: 4, w2: 5), 2) { dense, x in dense(x) })
  expectEqual((Dense.TangentVector(w1: 2), 4),
              gradient(at: Dense(w1: 4, w2: nil), 2) { dense, x in dense(x) })

  indirect enum Indirect {
    case e(Float, Enum)
    case indirect(Indirect)
  }

  func enum_indirect_notactive1(_ indirect: Indirect, _ x: Float) -> Float {
    switch indirect {
    case let .e(f, e):
      switch e {
      case .a: return x * f * enum_notactive1(e, x)
      case .b: return x * f * enum_notactive1(e, x)
      }
    case let .indirect(ind): return enum_indirect_notactive1(ind, x)
    }
  }
  do {
    let ind: Indirect = .e(10, .a(3))
    expectEqual(120, gradient(at: 2, in: { x in enum_indirect_notactive1(ind, x) }))
    expectEqual(120, gradient(at: 2, in: { x in enum_indirect_notactive1(.indirect(ind), x) }))
  }
}

ControlFlowTests.test("Loops") {
  func for_loop(_ x: Float) -> Float {
    var result = x
    for _ in 0..<2 {
      result = result * x
    }
    return result
  }
  expectEqual((8, 12), valueWithGradient(at: 2, in: for_loop))
  expectEqual((27, 27), valueWithGradient(at: 3, in: for_loop))

  func for_loop_nonactive_initial_value(_ x: Float) -> Float {
    var result: Float = 1
    for _ in 0..<2 {
      result = result * x
    }
    return result
  }
  expectEqual((4, 4), valueWithGradient(at: 2, in: for_loop_nonactive_initial_value))
  expectEqual((9, 6), valueWithGradient(at: 3, in: for_loop_nonactive_initial_value))

  func while_loop(_ x: Float) -> Float {
    var result = x
    var i = 0
    while i < 2 {
      result = result * x
      i += 1
    }
    return result
  }
  expectEqual((8, 12), valueWithGradient(at: 2, in: while_loop))
  expectEqual((27, 27), valueWithGradient(at: 3, in: while_loop))

  func while_loop_nonactive_initial_value(_ x: Float) -> Float {
    var result: Float = 1
    var i = 0
    while i < 2 {
      result = result * x
      i += 1
    }
    return result
  }
  expectEqual((4, 4), valueWithGradient(at: 2, in: while_loop_nonactive_initial_value))
  expectEqual((9, 6), valueWithGradient(at: 3, in: while_loop_nonactive_initial_value))

  func repeat_while_loop(_ x: Float) -> Float {
    var result = x
    var i = 0
    repeat {
      result = result * x
      i += 1
    } while i < 2
    return result
  }
  // FIXME(TF-584): Investigate incorrect (too big) gradient values for
  // repeat-while loops.
  // expectEqual((8, 12), valueWithGradient(at: 2, in: repeat_while_loop))
  // expectEqual((27, 27), valueWithGradient(at: 3, in: repeat_while_loop))
  expectEqual((8, 18), valueWithGradient(at: 2, in: repeat_while_loop))
  expectEqual((27, 36), valueWithGradient(at: 3, in: repeat_while_loop))

  func repeat_while_loop_nonactive_initial_value(_ x: Float) -> Float {
    var result: Float = 1
    var i = 0
    repeat {
      result = result * x
      i += 1
    } while i < 2
    return result
  }
  // FIXME(TF-584): Investigate incorrect (too big) gradient values for
  // repeat-while loops.
  // expectEqual((4, 4), valueWithGradient(at: 2, in: repeat_while_loop_nonactive_initial_value))
  // expectEqual((9, 6), valueWithGradient(at: 3, in: repeat_while_loop_nonactive_initial_value))
  expectEqual((4, 5), valueWithGradient(at: 2, in: repeat_while_loop_nonactive_initial_value))
  expectEqual((9, 7), valueWithGradient(at: 3, in: repeat_while_loop_nonactive_initial_value))

  func loop_continue(_ x: Float) -> Float {
    var result = x
    for i in 1..<10 {
      if i.isMultiple(of: 2) {
        continue
      }
      result = result * x
    }
    return result
  }
  expectEqual((64, 192), valueWithGradient(at: 2, in: loop_continue))
  expectEqual((729, 1458), valueWithGradient(at: 3, in: loop_continue))

  func loop_break(_ x: Float) -> Float {
    var result = x
    for i in 1..<10 {
      if i.isMultiple(of: 2) {
        continue
      }
      result = result * x
    }
    return result
  }
  expectEqual((64, 192), valueWithGradient(at: 2, in: loop_break))
  expectEqual((729, 1458), valueWithGradient(at: 3, in: loop_break))

  func nested_loop1(_ x: Float) -> Float {
    var outer = x
    for _ in 0..<2 {
      outer = outer * x

      var inner = outer
      var i = 0
      while i < 2 {
        inner = inner + x
        i += 1
      }
      outer = inner
    }
    return outer
  }
  expectEqual((20, 22), valueWithGradient(at: 2, in: nested_loop1))
  expectEqual((104, 66), valueWithGradient(at: 4, in: nested_loop1))

  func nested_loop2(_ x: Float, count: Int) -> Float {
    var outer = x
    outerLoop: for _ in 0..<count {
      outer = outer * x

      var inner = outer
      var i = 0
      while i < count {
        inner = inner + x
        i += 1

        switch Int(inner.truncatingRemainder(dividingBy: 7)) {
        case 0: break outerLoop
        case 1: break
        default: continue
        }
      }
      outer = inner
    }
    return outer
  }
  expectEqual((6, 5), valueWithGradient(at: 2, in: { x in nested_loop2(x, count: 1) }))
  expectEqual((20, 22), valueWithGradient(at: 2, in: { x in nested_loop2(x, count: 2) }))
  expectEqual((52, 80), valueWithGradient(at: 2, in: { x in nested_loop2(x, count: 3) }))
  expectEqual((24, 28), valueWithGradient(at: 2, in: { x in nested_loop2(x, count: 4) }))
}

runAllTests()
