// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-build-swift %s -Xfrontend -enable-experimental-keypaths -o %t/a.out
// RUN: %target-run %t/a.out
// REQUIRES: executable_test
// REQUIRES: PTRSIZE=64

// Disabled for now
// REQUIRES: rdar31776015

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
    let s_x = (#keyPath2(S<Int>, .x) as AnyKeyPath) as! WritableKeyPath<S<Int>, Int>
    let s_y = (#keyPath2(S<Int>, .y) as AnyKeyPath) as! WritableKeyPath<S<Int>, LifetimeTracked?>
    let s_z = (#keyPath2(S<Int>, .z) as AnyKeyPath) as! WritableKeyPath<S<Int>, Int>
    let s_p = (#keyPath2(S<Int>, .p) as AnyKeyPath) as! WritableKeyPath<S<Int>, Point>
    let s_p_x = (#keyPath2(S<Int>, .p.x) as AnyKeyPath) as! WritableKeyPath<S<Int>, Double>
    let s_p_y = (#keyPath2(S<Int>, .p.y) as AnyKeyPath) as! WritableKeyPath<S<Int>, Double>
    let s_c = (#keyPath2(S<Int>, .c) as AnyKeyPath) as! WritableKeyPath<S<Int>, C<Int>>
    let s_c_x = (#keyPath2(S<Int>, .c.x) as AnyKeyPath) as! ReferenceWritableKeyPath<S<Int>, Int>

    let c_x = (#keyPath2(C<Int>, .x) as AnyKeyPath) as! ReferenceWritableKeyPath<C<Int>, Int>
    let s_c_x_2 = s_c.appending(path: c_x)

    expectEqual(s_c_x, s_c_x_2)
    expectEqual(s_c_x_2, s_c_x)
    expectEqual(s_c_x.hashValue, s_c_x_2.hashValue)

    let point_x = (#keyPath2(Point, .x) as AnyKeyPath) as! WritableKeyPath<Point, Double>
    let point_y = (#keyPath2(Point, .y) as AnyKeyPath) as! WritableKeyPath<Point, Double>

    let s_p_x_2 = s_p.appending(path: point_x)
    let s_p_y_2 = s_p.appending(path: point_y)

    expectEqual(s_p_x, s_p_x_2)
    expectEqual(s_p_x_2, s_p_x)
    expectEqual(s_p_x_2.hashValue, s_p_x.hashValue)
    expectEqual(s_p_y, s_p_y_2)
    expectEqual(s_p_y_2, s_p_y)
    expectEqual(s_p_y_2.hashValue, s_p_y.hashValue)

    let ca_readOnly = (#keyPath2(ComputedA, .readOnly) as AnyKeyPath) as! KeyPath<ComputedA, ComputedB>
    let ca_nonmutating = (#keyPath2(ComputedA, .nonmutating) as AnyKeyPath) as! ReferenceWritableKeyPath<ComputedA, ComputedB>
    let ca_reabstracted = (#keyPath2(ComputedA, .reabstracted) as AnyKeyPath) as! ReferenceWritableKeyPath<ComputedA, () -> ()>

    let cb_readOnly = (#keyPath2(ComputedB, .readOnly) as AnyKeyPath) as! KeyPath<ComputedB, ComputedA>
    let cb_mutating = (#keyPath2(ComputedB, .mutating) as AnyKeyPath) as! WritableKeyPath<ComputedB, ComputedA>
    let cb_nonmutating = (#keyPath2(ComputedB, .nonmutating) as AnyKeyPath) as! ReferenceWritableKeyPath<ComputedB, ComputedA>
    let cb_reabstracted = (#keyPath2(ComputedB, .reabstracted) as AnyKeyPath) as! WritableKeyPath<ComputedB, () -> ()>
  
    let ca_readOnly_mutating = (#keyPath2(ComputedA, .readOnly.mutating) as AnyKeyPath) as! KeyPath<ComputedA, ComputedA>
    let cb_mutating_readOnly = (#keyPath2(ComputedB, .mutating.readOnly) as AnyKeyPath) as! KeyPath<ComputedB, ComputedB>
    let ca_readOnly_nonmutating = (#keyPath2(ComputedA, .readOnly.nonmutating) as AnyKeyPath) as! ReferenceWritableKeyPath<ComputedA, ComputedA>
    let cb_readOnly_reabstracted = (#keyPath2(ComputedB, .readOnly.reabstracted) as AnyKeyPath) as! ReferenceWritableKeyPath<ComputedB, () -> ()>

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
      let s_x = (#keyPath2(S<T>, .x) as AnyKeyPath) as! WritableKeyPath<S<T>, Int>
      let s_y = (#keyPath2(S<T>, .y) as AnyKeyPath) as! WritableKeyPath<S<T>, LifetimeTracked?>
      let s_z = (#keyPath2(S<T>, .z) as AnyKeyPath) as! WritableKeyPath<S<T>, T>
      let s_p = (#keyPath2(S<T>, .p) as AnyKeyPath) as! WritableKeyPath<S<T>, Point>
      let s_p_x = (#keyPath2(S<T>, .p.x) as AnyKeyPath) as! WritableKeyPath<S<T>, Double>
      let s_p_y = (#keyPath2(S<T>, .p.y) as AnyKeyPath) as! WritableKeyPath<S<T>, Double>
      let s_c = (#keyPath2(S<T>, .c) as AnyKeyPath) as! WritableKeyPath<S<T>, C<T>>
      let s_c_x = (#keyPath2(S<T>, .c.x) as AnyKeyPath) as! ReferenceWritableKeyPath<S<T>, Int>

      let c_x = (#keyPath2(C<T>, .x) as AnyKeyPath) as! ReferenceWritableKeyPath<C<T>, Int>
      let s_c_x_2 = s_c.appending(path: c_x)

      expectEqual(s_c_x, s_c_x_2)
      expectEqual(s_c_x_2, s_c_x)
      expectEqual(s_c_x.hashValue, s_c_x_2.hashValue)

      let point_x = (#keyPath2(Point, .x) as AnyKeyPath) as! WritableKeyPath<Point, Double>
      let point_y = (#keyPath2(Point, .y) as AnyKeyPath) as! WritableKeyPath<Point, Double>

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
  let s_c_x_int2 = #keyPath2(S<Int>, .c.x)
  expectEqual(s_c_x_int, s_c_x_int2)

  let s_c_x_string = testWithGenericParam(String.self)
  let s_c_x_string2 = #keyPath2(S<String>, .c.x)
  expectEqual(s_c_x_string, s_c_x_string2)

  let s_c_x_lt = testWithGenericParam(LifetimeTracked.self)
  let s_c_x_lt2 = #keyPath2(S<LifetimeTracked>, .c.x)
  expectEqual(s_c_x_lt, s_c_x_lt2)
}

runAllTests()
