// RUN: %target-run-simple-swift | FileCheck %s

struct S {
  var a, b : Int
  init(_ a : Int, _ b : Int) {
    self.a = a
    self.b = b
  }
  init(_ x:UnicodeScalar) {
    a = 219
    b = 912
    println("constructed \(x)")
  }
}

class C {
  var a, b : Int
  init(x:UnicodeScalar) {
    a = 20721
    b = 12702
    println("constructed \(x)")
  }
}

class D : C {
  init() {
    super.init(x: "z")
    println("...in bed")
  }
}

func println(s: S) {
  println("S(a=\(s.a), b=\(s.b))")
}

func println(c: C) {
  println("C(a=\(c.a), b=\(c.b))")
}

// CHECK: S(a=1, b=2)
println(S(1, 2))
// CHECK: constructed x
// CHECK: S(a=219, b=912)
println(S("x"))

// CHECK: constructed y
// CHECK: C(a=20721, b=12702)
println(C(x: "y"))

// CHECK: constructed z
// CHECK: ...in bed
// CHECK: C(a=20721, b=12702)
println(D())

class BaseWithDummyParameter {
  init() {
    fatalError("wrong init")
  }
  init(dummy: ()) {
    println("correct")
  }
}
class DerivedWithDummyParameter : BaseWithDummyParameter {
  init() {
    super.init(dummy: ())
  }
}

BaseWithDummyParameter(dummy: ()) // CHECK: correct
DerivedWithDummyParameter() // CHECK: correct

