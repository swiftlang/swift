// RUN: %target-run-simple-swift
// TODO(TF-1254): Support and test forward-mode differentiation.
// TODO(TF-1254): %target-run-simple-swift(-Xfrontend -enable-experimental-forward-mode-differentiation)
// REQUIRES: executable_test

import StdlibUnittest
import DifferentiationUnittest

var PropertyWrapperTests = TestSuite("PropertyWrapperDifferentiation")

@propertyWrapper
struct Wrapper<Value> {
  var wrappedValue: Value
}

struct Struct: Differentiable {
  @Wrapper @Wrapper var x: Tracked<Float> = 10
  @Wrapper var y: Tracked<Float> = 20
  var z: Tracked<Float> = 30
}

PropertyWrapperTests.test("SimpleStruct") {
  func getter(_ s: Struct) -> Tracked<Float> {
    return s.x
  }
  expectEqual(.init(x: 1, y: 0, z: 0), gradient(at: Struct(), in: getter))

  func setter(_ s: Struct, _ x: Tracked<Float>) -> Tracked<Float> {
    var s = s
    s.x = s.x * x * s.z
    return s.x
  }
  expectEqual((.init(x: 60, y: 0, z: 20), 300),
              gradient(at: Struct(), 2, in: setter))

  // TODO(SR-12640): Support `modify` accessors.
  /*
  func modify(_ s: Struct, _ x: Tracked<Float>) -> Tracked<Float> {
    var s = s
    s.x *= x * s.z
    return s.x
  }
  expectEqual((.init(x: 60, y: 0, z: 20), 300),
              gradient(at: Struct(), 2, in: modify))
  */
}

class Class: Differentiable {
  @Wrapper @Wrapper var x: Tracked<Float> = 10
  @Wrapper var y: Tracked<Float> = 20
  var z: Tracked<Float> = 30
}

// FIXME(SR-12641): Blocked by SILGen verification failure.
/*
PropertyWrapperTests.test("SimpleClass") {
  func getter(_ c: Class) -> Tracked<Float> {
    return c.x
  }
  expectEqual(.init(x: 1, y: 0, z: 0), gradient(at: Class(), in: getter))

  func setter(_ c: Class, _ x: Tracked<Float>) -> Tracked<Float> {
    var c = c
    c.x = c.x * x * c.z
    return c.x
  }
  expectEqual((.init(x: 60, y: 0, z: 20), 300),
              gradient(at: Class(), 2, in: setter))

  // TODO(SR-12640): Support `modify` accessors.
  /*
  func modify(_ c: Class, _ x: Tracked<Float>) -> Tracked<Float> {
    var c = c
    c.x *= x * c.z
    return c.x
  }
  expectEqual((.init(x: 60, y: 0, z: 20), 300),
              gradient(at: Class(), 2, in: modify))
  */
}
*/

struct GenericStruct<T> {
  @Wrapper var x: Tracked<Float> = 10
  @Wrapper @Wrapper @Wrapper var y: T
  var z: Tracked<Float> = 30
}
extension GenericStruct: Differentiable where T: Differentiable {}

PropertyWrapperTests.test("GenericStruct") {
  func getter<T>(_ s: GenericStruct<T>) -> T {
    return s.y
  }
  expectEqual(.init(x: 0, y: 1, z: 0),
              gradient(at: GenericStruct<Tracked<Float>>(y: 20), in: getter))

  func getter2<T>(_ s: GenericStruct<T>) -> Tracked<Float> {
    return s.x * s.z
  }
  expectEqual(.init(x: 30, y: 0, z: 10),
              gradient(at: GenericStruct<Tracked<Float>>(y: 20), in: getter2))

  func setter<T>(_ s: GenericStruct<T>, _ x: Tracked<Float>) -> Tracked<Float> {
    var s = s
    s.x = s.x * x * s.z
    return s.x
  }
  expectEqual((.init(x: 60, y: 0, z: 20), 300),
              gradient(at: GenericStruct<Tracked<Float>>(y: 20), 2, in: setter))

  // TODO(SR-12640): Support `modify` accessors.
  /*
  func modify<T>(_ s: GenericStruct<T>, _ x: Tracked<Float>) -> Tracked<Float> {
    var s = s
    s.x *= x * s.z
    return s.x
  }
  expectEqual((.init(x: 60, y: 0, z: 20), 300),
              gradient(at: GenericStruct<Tracked<Float>>(y: 1), 2, in: modify))
  */
}

runAllTests()
