// RUN: %swift -sil-i %s | FileCheck %s

struct S {
  var a, b : Int
  constructor(x:Char) {
    a = 219
    b = 912
    println("constructed \(x)")
  }
}

/* FIXME class constructors
class C {
  var a, b : Int
  constructor(x:Char) {
    a = 20721
    b = 12702
    println("constructed \(x)")
  }
}
*/

func println(s:S) {
  println("S(a=\(s.a), b=\(s.b))")
}

/*
func println(c:C) {
  println("C(a=\(c.a), b=\(c.b))")
}
*/

// CHECK: S(a=1, b=2)
println(S(1, 2))
// CHECK: constructed x
// CHECK: S(a=219, b=912)
println(S('x'))
/*
// C/HECK: constructed y
// C/HECK: S(a=20721, b=12702)
println(new C('y'))
*/
