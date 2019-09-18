// RUN: %target-typecheck-verify-swift

struct X { }
struct Y { }
struct Z { }

class A {
  func f(x: X) -> X { }
  func f(y: Y) -> Y { }

  func g(z: Z) -> X { } // expected-note 2{{found this candidate}}
  func g(z: Z) -> Y { } // expected-note 2{{found this candidate}}

  class func sf(x: X) -> X { }
  class func sf(y: Y) -> Y { }

  class func sg(z: Z) -> X { } // expected-note 2{{found this candidate}}
  class func sg(z: Z) -> Y { } // expected-note 2{{found this candidate}}

  func mixed(x: X) -> X { }
  class func mixed(y: Y) -> Y { }

  func mixed2(z: Z) -> X { }
  class func mixed2(z: Z) -> Y { }
}

func test_method_overload(_ a: A, x: X, y: Y) {
  var x1 = a.f(x: x)
  x1 = x
  _ = x1

  var y1 = a.f(y: y)
  y1 = y
  _ = y1
}

func test_method_overload_coerce(_ a: A, x: inout X, y: inout Y, z: Z) {
  var fail = a.g(z: z) // expected-error{{ambiguous use of 'g(z:)'}}
  x = a.g(z: z)
  y = a.g(z: z)
}

func test_method_value_coerce(_ a: A) {
  var _ : (X) -> X = a.f
  var _ : (A) -> (X) -> X = A.f
}

func test_static_method_overload(_ a: A, x: X, y: Y) {
  var x1 = A.sf(x: x)
  x1 = x
  _ = x1

  var y1 = A.sf(y: y)
  y1 = y
  _ = y1
}

func test_static_method_overload_coerce(_ a: A, x: inout X, y: inout Y, z: Z) {
  var fail = A.sg(z: z) // expected-error{{ambiguous use of 'sg(z:)'}}
  x = A.sg(z: z)
  y = A.sg(z: z)
}

func test_static_method_value_coerce(_ a: A) {
  var _ : (X) -> X = A.sf
  var _ : (Y) -> Y = A.sf
}

func test_mixed_overload(_ a: A, x: X, y: Y) {
  var x1 = a.mixed(x: x)
  x1 = x
  var y1 = a.mixed(y: y) // expected-error {{static member 'mixed' cannot be used on instance of type 'A'}} {{12-12=A.}}
  
  A.mixed(x) // expected-error{{cannot convert value of type 'X' to expected argument type 'A'}}
  var x2 = A.mixed(a)(x: x)
  x2 = x
  var y2 = A.mixed(y: y)
  y2 = y
}

func test_mixed_overload_coerce(_ a: A, x: inout X, y: Y, z: Z) {
  _ = a.mixed2(z: z)
  var y1 = A.mixed2(z: z)
  y1 = y
  _ = y1
  x = a.mixed2(z: z)
}

func test_mixed_method_value_coerce(_ a: A) {
  var _ : (X) -> X = a.mixed
  var _ : (Y) -> Y = A.mixed
  var _ : (Y) -> Y = a.mixed; // expected-error {{static member 'mixed' cannot be used on instance of type 'A'}} {{22-22=A.}}
  var _ : (A) -> (X) -> X = A.mixed
}

extension A {
  func test_method_overload(x: X, y: Y) {
    var x1 = self.f(x: x)
    x1 = x
    _ = x1
    var x2 = f(x: x)
    x2 = x
    _ = x2


    var y1 = self.f(y: y)
    y1 = y
    _ = y1
    var y2 = f(y: y)
    y2 = y
    _ = y2
  }

  func test_method_overload_coerce(x: inout X, y: inout Y, z: Z) {
    var fail = g(z: z) // expected-error{{ambiguous use of 'g(z:)'}}
    x = g(z: z)
    y = g(z: z)
  }

  func test_method_value_coerce() {
    var _ : (X) -> X = f
    var _ : (A) -> (X) -> X = A.f
    var _ : (A) -> (X) -> X = A.f
  }

  func test_mixed_overload_coerce(x: inout X, y: Y, z: Z) {
    _ = mixed2(z: z)
    x = mixed2(z: z)
  }

  func test_mixed_method_value_coerce() {
    var _ : (X) -> X = mixed
    var _ : (Y) -> Y = mixed; // expected-error {{static member 'mixed' cannot be used on instance of type 'A'}} {{24-24=A.}}
    var _ : (Y) -> Y = mixed; // expected-error {{static member 'mixed' cannot be used on instance of type 'A'}} {{24-24=A.}}
    var _ : (A) -> (X) -> X = A.mixed
  }

  class func test_method_overload_static(x: X, y: Y, z: Z) {
    var x1 = sf(x: x)
    x1 = x
    _ = x1

    var y1 = sf(y: y)
    y1 = y
    _ = y1
  }

  class func test_method_overload_coerce_static(x: inout X, y: inout Y, z: Z) {
    var fail = sg(z: z) // expected-error{{ambiguous use of 'sg(z:)'}}
    x = sg(z: z)
    y = sg(z: z)
  }

  class func test_method_value_coerce_static() {
    var _ : (X) -> X = sf
    var _ : (Y) -> Y = sf
  }

  class func test_mixed_overload_static(a: A, x: X, y: Y) {
    mixed(x) // expected-error{{cannot convert value of type 'X' to expected argument type 'A'}}
    var x2 = mixed(a)(x: x)
    x2 = x
    var y2 = mixed(y: y)
    y2 = y
  }

  class func test_mixed_overload_coerce_static(y: Y, z: Z) {
    var y1 = mixed2(z: z)
    y1 = y
    _ = y1
  }

  class func test_mixed_method_value_coerce_static() {
    var _ : (Y) -> Y = mixed
    var _ : (A) -> (X) -> X = mixed
  }
}

var clams : X

struct WeirdIvarLookupBehavior { 
  var clams : Y

  func f() {
    var _ : Y = clams
  }

  static func static_f() {
    // FIXME: These diagnostics still suck.
    var a : X = clams // expected-error{{member 'clams' cannot be used on type 'WeirdIvarLookupBehavior'}}
    // expected-error@-1 {{cannot convert value of type 'Y' to specified type 'X'}}
    var b : Y = clams // expected-error{{member 'clams' cannot be used on type 'WeirdIvarLookupBehavior'}}
  }
}

