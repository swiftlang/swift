// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-build-swift %s -Xfrontend -enable-experimental-keypath-components -o %t/a.out
// RUN: %target-run %t/a.out
// REQUIRES: executable_test

import StdlibUnittest

var keyPath = TestSuite("key paths")

final class C<T> {
  var x: Int
  var y: LifetimeTracked?
  var z: T

  init(x: Int, y: LifetimeTracked?, z: T) {
    self.x = x
    self.y = y
    self.z = z
  }
}

struct Point: Equatable {
  var x: Double
  var y: Double
  var trackLifetime = LifetimeTracked(123)
  
  init(x: Double, y: Double) {
    self.x = x
    self.y = y
  }
  
  static func ==(a: Point, b: Point) -> Bool {
    return a.x == b.x && a.y == b.y
  }
}

struct S<T: Equatable>: Equatable {
  var x: Int
  var y: LifetimeTracked?
  var z: T
  var p: Point
  var c: C<T>
  
  static func ==(a: S, b: S) -> Bool {
    return a.x == b.x
      && a.y === b.y
      && a.z == b.z
      && a.p == b.p
      && a.c === b.c
  }
}

final class ComputedA {
  var readOnly: ComputedB { fatalError() }
  var nonmutating: ComputedB {
    get { fatalError() }
    set { fatalError() }
  }
  var reabstracted: () -> () = {}
}

struct ComputedB {
  var readOnly: ComputedA { fatalError() }
  var mutating: ComputedA { 
    get { fatalError() }
    set { fatalError() }
  }
  var nonmutating: ComputedA {
    get { fatalError() }
    nonmutating set { fatalError() }
  }
  var reabstracted: () -> () = {}
}

keyPath.test("key path in-place instantiation") {
  for _ in 1...2 {
    let s_x = (\S<Int>.x as AnyKeyPath) as! WritableKeyPath<S<Int>, Int>
    let s_y = (\S<Int>.y as AnyKeyPath) as! WritableKeyPath<S<Int>, LifetimeTracked?>
    let s_z = (\S<Int>.z as AnyKeyPath) as! WritableKeyPath<S<Int>, Int>
    let s_p = (\S<Int>.p as AnyKeyPath) as! WritableKeyPath<S<Int>, Point>
    let s_p_x = (\S<Int>.p.x as AnyKeyPath) as! WritableKeyPath<S<Int>, Double>
    let s_p_y = (\S<Int>.p.y as AnyKeyPath) as! WritableKeyPath<S<Int>, Double>
    let s_c = (\S<Int>.c as AnyKeyPath) as! WritableKeyPath<S<Int>, C<Int>>
    let s_c_x = (\S<Int>.c.x as AnyKeyPath) as! ReferenceWritableKeyPath<S<Int>, Int>

    let c_x = (\C<Int>.x as AnyKeyPath) as! ReferenceWritableKeyPath<C<Int>, Int>
    let s_c_x_2 = s_c.appending(path: c_x)

    expectEqual(s_c_x, s_c_x_2)
    expectEqual(s_c_x_2, s_c_x)
    expectEqual(s_c_x.hashValue, s_c_x_2.hashValue)

    let point_x = (\Point.x as AnyKeyPath) as! WritableKeyPath<Point, Double>
    let point_y = (\Point.y as AnyKeyPath) as! WritableKeyPath<Point, Double>

    let s_p_x_2 = s_p.appending(path: point_x)
    let s_p_y_2 = s_p.appending(path: point_y)

    expectEqual(s_p_x, s_p_x_2)
    expectEqual(s_p_x_2, s_p_x)
    expectEqual(s_p_x_2.hashValue, s_p_x.hashValue)
    expectEqual(s_p_y, s_p_y_2)
    expectEqual(s_p_y_2, s_p_y)
    expectEqual(s_p_y_2.hashValue, s_p_y.hashValue)

    let ca_readOnly = (\ComputedA.readOnly as AnyKeyPath) as! KeyPath<ComputedA, ComputedB>
    let ca_nonmutating = (\ComputedA.nonmutating as AnyKeyPath) as! ReferenceWritableKeyPath<ComputedA, ComputedB>
    let ca_reabstracted = (\ComputedA.reabstracted as AnyKeyPath) as! ReferenceWritableKeyPath<ComputedA, () -> ()>

    let cb_readOnly = (\ComputedB.readOnly as AnyKeyPath) as! KeyPath<ComputedB, ComputedA>
    let cb_mutating = (\ComputedB.mutating as AnyKeyPath) as! WritableKeyPath<ComputedB, ComputedA>
    let cb_nonmutating = (\ComputedB.nonmutating as AnyKeyPath) as! ReferenceWritableKeyPath<ComputedB, ComputedA>
    let cb_reabstracted = (\ComputedB.reabstracted as AnyKeyPath) as! WritableKeyPath<ComputedB, () -> ()>
  
    let ca_readOnly_mutating = (\ComputedA.readOnly.mutating as AnyKeyPath) as! KeyPath<ComputedA, ComputedA>
    let cb_mutating_readOnly = (\ComputedB.mutating.readOnly as AnyKeyPath) as! KeyPath<ComputedB, ComputedB>
    let ca_readOnly_nonmutating = (\ComputedA.readOnly.nonmutating as AnyKeyPath) as! ReferenceWritableKeyPath<ComputedA, ComputedA>
    let cb_readOnly_reabstracted = (\ComputedB.readOnly.reabstracted as AnyKeyPath) as! ReferenceWritableKeyPath<ComputedB, () -> ()>

    let ca_readOnly_mutating2 = ca_readOnly.appending(path: cb_mutating)
    expectEqual(ca_readOnly_mutating, ca_readOnly_mutating2)
    expectEqual(ca_readOnly_mutating2, ca_readOnly_mutating)
    expectEqual(ca_readOnly_mutating.hashValue, ca_readOnly_mutating2.hashValue)

    let cb_mutating_readOnly2 = cb_mutating.appending(path: ca_readOnly)
    expectEqual(cb_mutating_readOnly, cb_mutating_readOnly2)
    expectEqual(cb_mutating_readOnly2, cb_mutating_readOnly)
    expectEqual(cb_mutating_readOnly.hashValue, cb_mutating_readOnly2.hashValue)

    let ca_readOnly_nonmutating2 = ca_readOnly.appending(path: cb_nonmutating)
    expectEqual(ca_readOnly_nonmutating, ca_readOnly_nonmutating2)
    expectEqual(ca_readOnly_nonmutating2, ca_readOnly_nonmutating)
    expectEqual(ca_readOnly_nonmutating.hashValue,
                ca_readOnly_nonmutating2.hashValue)

    let cb_readOnly_reabstracted2 = cb_readOnly.appending(path: ca_reabstracted)
    expectEqual(cb_readOnly_reabstracted,
                cb_readOnly_reabstracted2)
    expectEqual(cb_readOnly_reabstracted2,
                cb_readOnly_reabstracted)
    expectEqual(cb_readOnly_reabstracted2.hashValue,
                cb_readOnly_reabstracted.hashValue)
  }
}

keyPath.test("key path generic instantiation") {
  func testWithGenericParam<T: Equatable>(_: T.Type) -> ReferenceWritableKeyPath<S<T>, Int> {
    for i in 1...2 {
      let s_x = (\S<T>.x as AnyKeyPath) as! WritableKeyPath<S<T>, Int>
      let s_y = (\S<T>.y as AnyKeyPath) as! WritableKeyPath<S<T>, LifetimeTracked?>
      let s_z = (\S<T>.z as AnyKeyPath) as! WritableKeyPath<S<T>, T>
      let s_p = (\S<T>.p as AnyKeyPath) as! WritableKeyPath<S<T>, Point>
      let s_p_x = (\S<T>.p.x as AnyKeyPath) as! WritableKeyPath<S<T>, Double>
      let s_p_y = (\S<T>.p.y as AnyKeyPath) as! WritableKeyPath<S<T>, Double>
      let s_c = (\S<T>.c as AnyKeyPath) as! WritableKeyPath<S<T>, C<T>>
      let s_c_x = (\S<T>.c.x as AnyKeyPath) as! ReferenceWritableKeyPath<S<T>, Int>

      let c_x = (\C<T>.x as AnyKeyPath) as! ReferenceWritableKeyPath<C<T>, Int>
      let s_c_x_2 = s_c.appending(path: c_x)

      expectEqual(s_c_x, s_c_x_2)
      expectEqual(s_c_x_2, s_c_x)
      expectEqual(s_c_x.hashValue, s_c_x_2.hashValue)

      let point_x = (\Point.x as AnyKeyPath) as! WritableKeyPath<Point, Double>
      let point_y = (\Point.y as AnyKeyPath) as! WritableKeyPath<Point, Double>

      let s_p_x_2 = s_p.appending(path: point_x)
      let s_p_y_2 = s_p.appending(path: point_y)

      expectEqual(s_p_x, s_p_x_2)
      expectEqual(s_p_x_2, s_p_x)
      expectEqual(s_p_x_2.hashValue, s_p_x.hashValue)
      expectEqual(s_p_y, s_p_y_2)
      expectEqual(s_p_y_2, s_p_y)
      expectEqual(s_p_y_2.hashValue, s_p_y.hashValue)

      if i == 2 { return s_c_x }
    }
    fatalError()
  }
  let s_c_x_int = testWithGenericParam(Int.self)
  let s_c_x_int2 = \S<Int>.c.x
  expectEqual(s_c_x_int, s_c_x_int2)

  let s_c_x_string = testWithGenericParam(String.self)
  let s_c_x_string2 = \S<String>.c.x
  expectEqual(s_c_x_string, s_c_x_string2)

  let s_c_x_lt = testWithGenericParam(LifetimeTracked.self)
  let s_c_x_lt2 = \S<LifetimeTracked>.c.x
  expectEqual(s_c_x_lt, s_c_x_lt2)
}

protocol P {}

struct TestComputed: P {
  static var numNonmutatingSets = 0
  static var numMutatingSets = 0

  static func resetCounts() {
    numNonmutatingSets = 0
    numMutatingSets = 0
  }

  var canary = LifetimeTracked(0)

  var readonly: LifetimeTracked {
    return LifetimeTracked(1)
  }
  var nonmutating: LifetimeTracked {
    get {
      return LifetimeTracked(2)
    }
    nonmutating set { TestComputed.numNonmutatingSets += 1 }
  }
  var mutating: LifetimeTracked {
    get {
      return LifetimeTracked(3)
    }
    set {
      canary = newValue
    }
  }
}

extension P {
  var readonlyProtoExt: Self { return self }
  var mutatingProtoExt: Self {
    get { return self }
    set { self = newValue }
  }
}

keyPath.test("computed properties") {
  var test = TestComputed()

  do {
    let tc_readonly = \TestComputed.readonly
    expectTrue(test[keyPath: tc_readonly] !== test[keyPath: tc_readonly])
    expectEqual(test[keyPath: tc_readonly].value,
                test[keyPath: tc_readonly].value)
  }

  do {
    let tc_nonmutating = \TestComputed.nonmutating
    expectTrue(test[keyPath: tc_nonmutating] !== test[keyPath: tc_nonmutating])
    expectEqual(test[keyPath: tc_nonmutating].value,
                test[keyPath: tc_nonmutating].value)
    TestComputed.resetCounts()
    test[keyPath: tc_nonmutating] = LifetimeTracked(4)
    expectEqual(TestComputed.numNonmutatingSets, 1)
  }

  do {
    let tc_mutating = \TestComputed.mutating
    expectTrue(test[keyPath: tc_mutating] !== test[keyPath: tc_mutating])
    expectEqual(test[keyPath: tc_mutating].value,
                test[keyPath: tc_mutating].value)
    let newObject = LifetimeTracked(5)
    test[keyPath: tc_mutating] = newObject
    expectTrue(test.canary === newObject)
  }

  do {
    let tc_readonlyProtoExt = \TestComputed.readonlyProtoExt
    expectTrue(test.canary === test[keyPath: tc_readonlyProtoExt].canary)
  }

  do {
    let tc_mutatingProtoExt = \TestComputed.mutatingProtoExt
    expectTrue(test.canary === test[keyPath: tc_mutatingProtoExt].canary)
    let oldTest = test
    test[keyPath: tc_mutatingProtoExt] = TestComputed()
    expectTrue(oldTest.canary !== test.canary)
    expectTrue(test.canary === test[keyPath: tc_mutatingProtoExt].canary)
  }
}

runAllTests()
