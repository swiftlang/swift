// RUN: %target-run-simple-swift | FileCheck %s

class A {
  init() { println("a") }
  init(_ x:Int) { println("b") }
  init<T>(_ x:Int, _ y:T) { println("c") }
}

class B<T> {
  init() { println("d") }
  init(_ x:Int) { println("e") }
  init(_ x:T) { println("f") }

  init<U>(_ x:Int, _ y:U) { println("g") }
  init<U>(_ x:T, _ y:U) { println("h") }
}

protocol Runcible {}

class C<T : Runcible> {
  init() { println("i") }
  init(_ x:Int) { println("j") }
  init(_ x:T) { println("k") }
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
