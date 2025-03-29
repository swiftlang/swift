// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// This test fails on arm64e with a pointer auth failure.
// XFAIL: CPU=arm64e

import _Differentiation
import StdlibUnittest

var ModifyAccessorTests = TestSuite("ModifyAccessor")

ModifyAccessorTests.test("SimpleModifyAccessor") {
  struct S: Differentiable {
    private var _x : Float

    func _endMutation() {}

    var x: Float {
      get{_x}
      set(newValue) { _x = newValue }
      _modify {
        defer { _endMutation() }
        if (x > 0) {
          yield &_x
        } else {
          yield &_x
        }
      }
    }

    init(_ x : Float) {
      self._x = x
    }
  }

  func modify_struct(_ x : Float) -> Float {
    var s = S(x)
    s.x *= s.x
    return s.x
  }

  expectEqual((100, 20), valueWithGradient(at: 10, of: modify_struct))
}

ModifyAccessorTests.test("GenericModifyAccessor") {
  struct S<T : Differentiable & SignedNumeric & Comparable>: Differentiable {
    private var _x : T

    func _endMutation() {}

    var x: T {
      get{_x}
      set(newValue) { _x = newValue }
      _modify {
        defer { _endMutation() }
        if (x > -x) {
          yield &_x
        } else {
          yield &_x
        }
      }
    }

    init(_ x : T) {
      self._x = x
    }
  }

  func modify_struct(_ x : Float) -> Float {
    var s = S<Float>(x)
    s.x *= s.x
    return s.x
  }

  expectEqual((100, 20), valueWithGradient(at: 10, of: modify_struct))
}


runAllTests()

