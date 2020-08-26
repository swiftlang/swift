// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-forward-mode-differentiation)
// REQUIRES: executable_test

import StdlibUnittest
import DifferentiationUnittest

var ForwardModeInoutTests = TestSuite("ForwardModeInoutDifferentiation")

//===----------------------------------------------------------------------===//
// Inout tests.
//===----------------------------------------------------------------------===//

import DifferentiationUnittest
import StdlibUnittest

ForwardModeInoutTests.test("Float.+=") {
  func mutatingAddWrapper(_ x: Float, _ y: Float) -> Float {
    var result: Float = x
    result += y
    return result
  }
  expectEqual(20, differential(at: 4, 5, in: mutatingAddWrapper)(10, 10))
}

ForwardModeInoutTests.test("Float.-=") {
  func mutatingSubtractWrapper(_ x: Float, _ y: Float) -> Float {
    var result: Float = x
    result -= y
    return result
  }
  expectEqual(0, differential(at: 4, 5, in: mutatingSubtractWrapper)(10, 10))
  expectEqual(10, differential(at: 4, 5, in: mutatingSubtractWrapper)(20, 10))
}

ForwardModeInoutTests.test("Float.*=") {
  func mutatingMultiplyWrapper(_ x: Float, _ y: Float) -> Float {
    var result: Float = x
    result *= y
    return result
  }
  expectEqual(22, differential(at: 4, 5, in: mutatingMultiplyWrapper)(2, 3))
}

ForwardModeInoutTests.test("Float./=") {
  func mutatingDivideWrapper(_ x: Float, _ y: Float) -> Float {
    var result: Float = x
    result /= y
    return result
  }
  expectEqual(-1, differential(at: 2, 3, in: mutatingDivideWrapper)(3, 9))
}

// Simplest possible `inout` parameter differentiation.
ForwardModeInoutTests.test("InoutIdentity") {
  // Semantically, an empty function with an `inout` parameter is an identity
  // function.
  func inoutIdentity(_ x: inout Float) {}

  func identity(_ x: Float) -> Float {
    var result = x
    inoutIdentity(&result)
    return result
  }
  expectEqual(1, derivative(at: 10, in: identity))
  expectEqual(10, differential(at: 10, in: identity)(10))

  func inoutIdentityGeneric<T: Differentiable>(_ x: inout T) {}

  func identityGeneric<T: Differentiable>(_ x: T) -> T {
    var result: T = x
    inoutIdentityGeneric(&result)
    return result
  }
  expectEqual(1, derivative(at: 10.0, in: identityGeneric))
  expectEqual(10, differential(at: 10.0, in: identityGeneric)(10))
}

ForwardModeInoutTests.test("MultipleInoutParams") {
  func swap<T: Differentiable>(_ x: inout T, _ y: inout T) {
    let z = x
    x = y
    y = z
  }

  func first<T: Differentiable>(_ x: T, _ y: T) -> T {
    var p1 = x
    var p2 = y
    swap(&p1, &p2)
    return p2
  }
  expectEqual(1, differential(at: 1, 1, in: first)(1, 2))

  func second<T: Differentiable>(_ x: T, _ y: T) -> T {
    var p1 = x
    var p2 = y
    swap(&p1, &p2)
    return p1
  }
  expectEqual(2, differential(at: 1, 1, in: second)(1, 2))
}

ForwardModeInoutTests.test("StructMutatingMethod") {
  struct Mut: Differentiable {
    var x: Float

    mutating func add(_ y: Float) {
      self.x += y
    }

    mutating func addMut(_ m: Mut) {
      self.x += m.x
    }
  }

  func identity(_ y: Float) -> Float {
    var mut = Mut(x: 0.0)
    mut.add(y)
    return mut.x
  }
  expectEqual(1, derivative(at: 1, in: identity))

  func identity2(_ y: Float) -> Float {
    var mut = Mut(x: 0.0)
    let mut2 = Mut(x: y)
    mut.addMut(mut2)
    return mut.x
  }
  expectEqual(1, derivative(at: 1, in: identity2))

  func double(_ y: Float) -> Float {
    var mut = Mut(x: y)
    mut.add(y)
    return mut.x
  }
  expectEqual(2, derivative(at: 1, in: double))

  func double2(_ y: Float) -> Float {
    var mut = Mut(x: y)
    let mut2 = Mut(x: y)
    mut.addMut(mut2)
    return mut.x
  }
  expectEqual(2, derivative(at: 1, in: double2))

  func square(_ y: Float) -> Float {
    var mut = Mut(x: 0.0)
    mut.add(y * y)
    return mut.x
  }
  expectEqual(6, derivative(at: 3, in: square))

  func square2(_ y: Float) -> Float {
    var mut = Mut(x: 0.0)
    let mut2 = Mut(x: y * y)
    mut.addMut(mut2)
    return mut.x
  }
  expectEqual(6, derivative(at: 3, in: square2))
}

runAllTests()
