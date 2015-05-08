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
    print("constructed \(x)")
  }
}

class C {
  var a, b : Int
  init(x:UnicodeScalar) {
    a = 20721
    b = 12702
    print("constructed \(x)")
  }
}

class D : C {
  init() {
    super.init(x: "z")
    print("...in bed")
  }
}

func print(s: S) {
  print("S(a=\(s.a), b=\(s.b))")
}

func print(c: C) {
  print("C(a=\(c.a), b=\(c.b))")
}

// CHECK: S(a=1, b=2)
print(S(1, 2))
// CHECK: constructed x
// CHECK: S(a=219, b=912)
print(S("x"))

// CHECK: constructed y
// CHECK: C(a=20721, b=12702)
print(C(x: "y"))

// CHECK: constructed z
// CHECK: ...in bed
// CHECK: C(a=20721, b=12702)
print(D())

class BaseWithDummyParameter {
  init() {
    fatalError("wrong init")
  }
  init(dummy: ()) {
    print("correct")
  }
}
class DerivedWithDummyParameter : BaseWithDummyParameter {
  override init() {
    super.init(dummy: ())
  }
}

BaseWithDummyParameter(dummy: ()) // CHECK: correct
DerivedWithDummyParameter() // CHECK: correct

