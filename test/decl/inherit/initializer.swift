// RUN: %target-parse-verify-swift

class A {
  init(int i: Int) { }
  init(double d: Double) { }
  convenience init(float f: Float) {
    self.init(double: Double(f))
  }
}

// Implicit overriding of subobject initializers
class B : A {
  var x = "Hello"
  var (y, z) = (1, 2)
}

func testB() {
  var b1 = B(int: 5)
  var b2 = B(double: 2.71828)
  var b3 = B(float: 3.14159) 
}

// Okay to have nothing
class C : B {
}

func testC() {
  var c1 = C(int: 5)
  var c2 = C(double: 2.71828)
  var c3 = C(float: 3.14159) 
}

// Okay to add convenience initializers.
class D : C {
  convenience init(string s: String) {
    self.init(int: s.toInt()!)
  }
}

func testD() {
  var d1 = D(int: 5)
  var d2 = D(double: 2.71828)
  var d3 = D(float: 3.14159) 
  var d4 = D(string: "3.14159") 
}

// Adding a subobject initializer prevents inheritance of subobject
// initializers.
class NotInherited1 : D {
  override init(int i: Int) {
    super.init(int: i)
  }

  convenience init(float f: Float) {
    self.init(int: Int(f))
  }
}

func testNotInherited1() {
  var n1 = NotInherited1(int: 5)
  var n2 = NotInherited1(double: 2.71828) // expected-error{{incorrect argument label in call (have 'double:', expected 'float:')}}
}

class NotInherited1Sub : NotInherited1 {
  override init(int i: Int) {
    super.init(int: i)
  }
}

func testNotInherited1Sub() {
  var n1 = NotInherited1Sub(int: 5)
  var n2 = NotInherited1Sub(float: 3.14159)
  var n3 = NotInherited1Sub(double: 2.71828) // expected-error{{incorrect argument label in call (have 'double:', expected 'float:')}}
}

// Having a stored property without an initial value prevents
// inheritance of initializers.
class NotInherited2 : D { // expected-error{{class 'NotInherited2' has no initializers}}
  var a: Int // expected-note{{stored property 'a' without initial value prevents synthesized initializers}}{{13-13= = 0}}
  var b: Int // expected-note{{stored property 'b' without initial value prevents synthesized initializers}}{{13-13= = 0}}
    , c: Int  // expected-note{{stored property 'c' without initial value prevents synthesized initializers}}{{13-13= = 0}}
  
  var (d, e): (Int, String) // expected-note{{stored properties 'd' and 'e' without initial values prevent synthesized initializers}}{{28-28= = (0, "")}}
  var (f, (g, h)): (Int?, (Float, String)) // expected-note{{stored properties 'f', 'g', and 'h' without initial values prevent synthesized initializers}}{{43-43= = (nil, (0.0, ""))}}
  var (w, i, (j, k)): (Int?, Float, (String, D)) // expected-note{{stored properties 'w', 'i', 'j', and others without initial values prevent synthesized initializers}}
}

func testNotInherited2() {
  var n1 = NotInherited2(int: 5) // expected-error{{'NotInherited2' cannot be constructed because it has no accessible initializers}}
  var n2 = NotInherited2(double: 2.72828) // expected-error{{'NotInherited2' cannot be constructed because it has no accessible initializers}}
}

// Inheritance of unnamed parameters.
class SuperUnnamed {
  init(int _: Int) { }
  init(_ : Double) { }

  init(var string _: String) { }
  init(var _ : Float) { }
}

class SubUnnamed : SuperUnnamed { }

func testSubUnnamed(i: Int, d: Double, s: String, f: Float) {
  var su1 = SubUnnamed(int: i)
  var su2 = SubUnnamed(d)
  var su3 = SubUnnamed(string: s)
  var su4 = SubUnnamed(f)
}

// FIXME: <rdar://problem/16331406> Implement inheritance of variadic designated initializers
class SuperVariadic {
  init(ints: Int...) { } // expected-note{{variadic superclass initializer defined here}}
  init(_ : Double...) { } // expected-note{{variadic superclass initializer defined here}}

  init(s: String, ints: Int...) { } // expected-note{{variadic superclass initializer defined here}}
  init(s: String, _ : Double...) { } // expected-note{{variadic superclass initializer defined here}}
}

class SubVariadic : SuperVariadic { } // expected-warning 4{{synthesizing a variadic inherited initializer for subclass 'SubVariadic' is unsupported}}

