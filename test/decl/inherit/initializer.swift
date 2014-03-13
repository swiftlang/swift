// RUN: %swift -parse %s -verify

class A {
  init withInt(i: Int) { }
  init withDouble(d: Double) { }
  init withFloat(f: Float) -> Self {
    self.init(Double(f))
  }
}

// Implicit overriding of subobject initializers
class B : A {
  var x = "Hello"
  var (y, z) = (1, 2)
}

func testB() {
  var b1 = B(withInt: 5)
  var b2 = B(withDouble: 2.71828)
  var b3 = B(withFloat: 3.14159) 
}

// Okay to have nothing
class C : B {
}

func testC() {
  var c1 = C(withInt: 5)
  var c2 = C(withDouble: 2.71828)
  var c3 = C(withFloat: 3.14159) 
}

// Okay to add convenience initializers.
class D : C {
  init withString(s: String) -> Self {
    self.init(withInt: s.toInt()!)
  }
}

func testD() {
  var d1 = D(withInt: 5)
  var d2 = D(withDouble: 2.71828)
  var d3 = D(withFloat: 3.14159) 
  var d4 = D(withString: "3.14159") 
}

// Adding a subobject initializer prevents inheritance of subobject
// initializers.
class NotInherited1 : D {
  init withInt(i: Int) {
    super.init(withInt: i)
  }
}

func testNotInherited1() {
  var n1 = NotInherited1(withInt: 5)
  var n2 = NotInherited1(withDouble: 2.71828) // expected-error{{expression does not type-check}}
}

// Having a stored property without an initial value prevents
// inheritance of initializers.
class NotInherited2 : D {
  var i: Int
}

func testNotInherited2() {
  var n1 = NotInherited2(withInt: 5) // expected-error{{'NotInherited2' is not constructible with '(withInt: $T1)'}}
  var n2 = NotInherited2(withDouble: 2.72828) // expected-error{{'NotInherited2' is not constructible with '(withDouble: $T1)'}}
}
