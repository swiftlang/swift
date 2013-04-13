// RUN: %swift -i %s | FileCheck %s

class A {
  constructor() { println("a") }
  constructor(x:Int) { println("b") }
  constructor<T>(x:Int, y:T) { println("c") }
}

class B<T> {
  constructor() { println("d") }
  constructor(x:Int) { println("e") }
  constructor(x:T) { println("f") }

  constructor<U>(x:Int, y:U) { println("g") }
  constructor<U>(x:T, y:U) { println("h") }
}

protocol Runcible {}

class C<T : Runcible> {
  constructor() { println("i") }
  constructor(x:Int) { println("j") }
  constructor(x:T) { println("k") }
}

// CHECK: a
new A()
// CHECK: b
new A(1)
// CHECK: c
new A(1, '2')

typealias BChar = B<Char>
// CHECK: d
new BChar()
// CHECK: e
new BChar(1)
// CHECK: f
new BChar('2')
// CHECK: g
new BChar(1, "2")
// CHECK: h
new BChar('1', "2")

// <rdar://problem/12965934> Destructors for classes with constrained type parameters

struct Hat : Runcible {}

typealias CHat = C<Hat>
// CHECK: i
new CHat()
// CHECK: j
new CHat(1)
// CHECK: k
new CHat(Hat())
