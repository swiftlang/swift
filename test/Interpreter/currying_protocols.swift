// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

protocol P {
  func scale(x : Int)(y : Int) -> Int
}

struct S : P  {
  var offset : Int
  func scale(x : Int)(y : Int) -> Int {
    return offset + x * y
  }
}

func foo<T : P>(t : T) {
  print("\(t.scale(7)(y: 13))\n", appendNewline: false)
}

foo(S(offset: 4))
// CHECK: 95

let p : P = S(offset: 4)
print("\(p.scale(5)(y: 7))\n", appendNewline: false)
// CHECK: 39

func culDeSac<T>(t: T) {}
func roundabout<T>(t: T, _ u: T -> ()) { u(t) }

protocol Proto {
  typealias Category
  func f(t: Self)(i: Category, j: Category)
  static func ff()(i: Category, j: Category)
}

class Q : Proto {
  typealias Category = Int
  var q: Int
  init(q: Int) { self.q = q }
  func f(t: Q)(i: Int, j: Int) { print(q * t.q + i * j) }
  static func ff()(i: Int, j: Int) { print(i * j) }
}

func suburbanJungle<T : Proto where T.Category == Int>(t: T) {
  roundabout(T.ff) { $0()(i: 2, j: 10) }
  roundabout(T.ff()) { $0(i: 4, j: 10) }
  culDeSac(T.ff()(i: 6, j: 10))
  roundabout(T.f) { $0(t)(t)(i: 1, j: 2) }
  roundabout(T.f(t)) { $0(t)(i: 3, j: 4) }
  roundabout(T.f(t)(t)) { $0(i: 5, j: 6) }
  culDeSac(T.f(t)(t)(i: 7, j: 8))
  roundabout(t.f) { $0(t)(i: 9, j: 10) }
  roundabout(t.f(t)) { $0(i: 11, j: 12) }
  culDeSac(t.f(t)(i: 13, j: 14))
}

suburbanJungle(Q(q: 3))

// CHECK: 20
// CHECK: 40
// CHECK: 60
// CHECK: 11
// CHECK: 21
// CHECK: 39
// CHECK: 65
// CHECK: 99
// CHECK: 141
// CHECK: 191
