// RUN: %target-run-simple-swift | FileCheck %s

class A {
  init() { print("a") }
  init(_ x:Int) { print("b") }
  init<T>(_ x:Int, _ y:T) { print("c") }
}

class B<T> {
  init() { print("d") }
  init(_ x:Int) { print("e") }
  init(_ x:T) { print("f") }

  init<U>(_ x:Int, _ y:U) { print("g") }
  init<U>(_ x:T, _ y:U) { print("h") }
}

protocol Runcible {}

class C<T : Runcible> {
  init() { print("i") }
  init(_ x:Int) { print("j") }
  init(_ x:T) { print("k") }
}

// CHECK: a
A()
// CHECK: b
A(1)
// CHECK: c
A(1, "2")

typealias BChar = B<UnicodeScalar>
// CHECK: d
BChar()
// CHECK: e
BChar(1)
// CHECK: f
BChar("2")
// CHECK: g
BChar(1, "2")
// CHECK: h
BChar("1", "2")

// <rdar://problem/12965934> Destructors for classes with constrained type parameters

struct Hat : Runcible {}

typealias CHat = C<Hat>
// CHECK: i
CHat()
// CHECK: j
CHat(1)
// CHECK: k
CHat(Hat())
