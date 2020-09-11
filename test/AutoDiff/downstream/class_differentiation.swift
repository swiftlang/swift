// RUN: %target-run-simple-swift
// NOTE: Verify whether forward-mode differentiation crashes. It currently does.
// RUN: not --crash %target-swift-frontend -enable-experimental-forward-mode-differentiation -emit-sil %s
// REQUIRES: executable_test

import StdlibUnittest
import DifferentiationUnittest

var ClassTests = TestSuite("ClassDifferentiation")

ClassTests.test("TrivialMember") {
  final class C: Differentiable {
    @differentiable
    var float: Float

    @noDerivative
    final var noDerivative: Float = 1

    @differentiable
    init(_ float: Float) {
      self.float = float
    }

    @differentiable
    convenience init(convenience x: Float) {
      self.init(x)
    }

    @differentiable
    func method(_ x: Float) -> Float {
      x * float
    }

    @differentiable
    func testNoDerivative() -> Float {
      noDerivative
    }

    @differentiable
    static func controlFlow(_ c1: C, _ c2: C, _ flag: Bool) -> Float {
      var result: Float = 0
      if flag {
        var c3 = C(c1.float * c2.float)
        result = c3.float
      } else {
        result = c2.float * c1.float
      }
      return result
    }
  }
  // Test class initializer differentiation.
  expectEqual(10, pullback(at: 3, in: { C($0) })(.init(float: 10)))
  expectEqual(10, pullback(at: 3, in: { C(convenience: $0) })(.init(float: 10)))
  // Test class method differentiation.
  expectEqual((.init(float: 3), 10), gradient(at: C(10), 3, in: { c, x in c.method(x) }))
  expectEqual(.init(float: 0), gradient(at: C(10), in: { c in c.testNoDerivative() }))
  expectEqual((.init(float: 20), .init(float: 10)),
              gradient(at: C(10), C(20), in: { c1, c2 in C.controlFlow(c1, c2, true) }))
}

ClassTests.test("NontrivialMember") {
  final class C: Differentiable {
    @differentiable
    var float: Tracked<Float>

    @differentiable
    init(_ float: Tracked<Float>) {
      self.float = float
    }

    @differentiable
    func method(_ x: Tracked<Float>) -> Tracked<Float> {
      x * float
    }

    @differentiable
    static func controlFlow(_ c1: C, _ c2: C, _ flag: Bool) -> Tracked<Float> {
      var result: Tracked<Float> = 0
      if flag {
        result = c1.float * c2.float
      } else {
        result = c2.float * c1.float
      }
      return result
    }
  }
  // Test class initializer differentiation.
  expectEqual(10, pullback(at: 3, in: { C($0) })(.init(float: 10)))
  // Test class method differentiation.
  expectEqual((.init(float: 3), 10), gradient(at: C(10), 3, in: { c, x in c.method(x) }))
  expectEqual((.init(float: 20), .init(float: 10)),
              gradient(at: C(10), C(20), in: { c1, c2 in C.controlFlow(c1, c2, true) }))
}

ClassTests.test("GenericNontrivialMember") {
  final class C<T: Differentiable>: Differentiable where T == T.TangentVector {
    @differentiable
    var x: Tracked<T>

    @differentiable
    init(_ x: T) {
      self.x = Tracked(x)
    }

    @differentiable
    convenience init(convenience x: T) {
      self.init(x)
    }
  }
  // Test class initializer differentiation.
  expectEqual(10, pullback(at: 3, in: { C<Float>($0) })(.init(x: 10)))
  expectEqual(10, pullback(at: 3, in: { C<Float>(convenience: $0) })(.init(x: 10)))
}

// TF-1149: Test class with loadable type but address-only `TangentVector` type.
// TODO(TF-1149): Uncomment when supported.
/*
ClassTests.test("AddressOnlyTangentVector") {
  final class C<T: Differentiable>: Differentiable {
    @differentiable
    var stored: T

    @differentiable
    init(_ stored: T) {
      self.stored = stored
    }

    @differentiable
    func method(_ x: T) -> T {
      stored
    }
  }
  // Test class initializer differentiation.
  expectEqual(10, pullback(at: 3, in: { C<Float>($0) })(.init(float: 10)))
  // Test class method differentiation.
  expectEqual((.init(stored: Float(3)), 10),
              gradient(at: C<Float>(3), 3, in: { c, x in c.method(x) }))
}
*/

// TF-1175: Test whether class-typed arguments are not marked active.
ClassTests.test("ClassArgumentActivity") {
  class C: Differentiable {
    @differentiable
    var x: Float

    init(_ x: Float) {
      self.x = x
    }

    // Note: this method mutates `self`. However, since `C` is a class, the
    // method type does not involve `inout` arguments: `(C) -> () -> ()`.
    func square() {
      x *= x
    }
  }

  // Returns `x * x`.
  func squared(_ x: Float) -> Float {
    var c = C(x)
    c.square() // FIXME(TF-1175): doesn't get differentiated!
    return c.x
  }
  // FIXME(TF-1175): Find a robust solution so that derivatives are correct.
  // expectEqual((100, 20), valueWithGradient(at: 10, in: squared))
  expectEqual((100, 1), valueWithGradient(at: 10, in: squared))
}

runAllTests()
