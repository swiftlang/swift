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
  /* FIXME: Generic ctors of generic classes currently assert out.
  rdar://problem/13083304
  constructor<U>(x:Int, y:U) { println("g") }
  constructor<U>(x:T, y:U) { println("h") }
  */
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
/* FIXME
// C/HECK: g
new BChar(1, "2")
// C/HECK: h
new BChar('1', "2")
*/
