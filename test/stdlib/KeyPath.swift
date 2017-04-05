// RUN: %target-run-simple-swift
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

keyPath.test("key path instantiation") {
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
}


runAllTests()
