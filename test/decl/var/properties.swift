// RUN: %swift %s -verify

struct X { }
var _x : X

// Basic parsing
var x0 : X {
  return _x
}

var x1 : X {
  get: return _x
  set: _x = value
}

var x2 : X {
  set (val): _x = val
  get: return _x
}

var x3 : X {
  get: return _x
}

// Parsing problems
// FIXME: Redundant error below
var x4 { // expected-error{{variable with getter/setter must have an explicit type}} expected-error{{type annotation missing in pattern}}
  get: return _x
}

var (x5 : Int) { // expected-error{{getter/setter can only be defined for a single variable}}
  get: return _x
}

var x6 : X {
  get: return _x // expected-note{{previous definition of getter is here}}
  set: _x = value // expected-note{{previous definition of setter is here}}
  get: return _x // expected-error{{duplicate definition of getter}}
  set (val): _x = val // expected-error{{duplicate definition of setter}}
}

var x7 : X {
    get blah wibble // expected-error{{expected ':' to start getter definition}}
}

var x8 : X {
    set blah wibble // expected-error{{expected ':' to start setter definition}}
}

var x9 : X {
    get ( ): // expected-error{{expected ':' to start getter definition}}
}

var x10 : X {
    set ( : ): // expected-error{{expected the name of the setter value}}
    get:
}

var x11 : X {
    set: // expected-error{{variable with a setter must also have a getter}}
}

var x12 : X { // expected-error{{variable with getter/setter cannot have an initial value}}
    get:
} = X()

// Type checking problems
struct Y { }
var y : Y
var x20 : X {
    get: return y // expected-error{{'Y' is not convertible to 'X'}}
    set: y = value // expected-error{{'X' is not convertible to 'Y'}}
}

var x21 : X {
    get: return y // expected-error{{'Y' is not convertible to 'X'}}
    set (val): y = val // expected-error{{'X' is not convertible to 'Y'}}
}

var x22 : @inout X { // expected-error{{type '@inout X' of variable is not materializable}}
    get: return _x // expected-error{{expression does not type-check}}
}

var x23 : Int, x24 : Int { // expected-error{{'var' declarations with multiple variables cannot have explicit getters/setters}}
    return 42
}

var x25 : Int { // expected-error{{'var' declarations with multiple variables cannot have explicit getters/setters}}
    return 42
}, x26 : Int

// Properties of struct/enum/extensions
struct S {
  var _backed_x : X,
  _backed_x2 : X
  var x : X {
    get: return _backed_x
    set(val): _backed_x = val
  }
}

extension S {
  var x2 : X {
    get: return self._backed_x2
    set: _backed_x2 = value
  }

  var x3 : X {
    get: return self._backed_x2
  }
}

// Reading/writing properties
def accept_x(x: X) { }
def accept_x_inout(x: @inout X) { }

def test_global_properties(x: X) {
  accept_x(x1)
  accept_x(x2)
  accept_x(x3)

  x1 = x
  x2 = x
  x3 = x // expected-error{{cannot assign to read-only variable or subscript}}

  accept_x_inout(&x1)
  accept_x_inout(&x2)
  accept_x_inout(&x3) // expected-error{{expression does not type-check}}
}

def getS() -> S { var s : S; return s }

def test_extension_properties(s: S, x: X) {
  accept_x(s.x)
  accept_x(s.x2)
  accept_x(s.x3)

  accept_x(getS().x)
  accept_x(getS().x2)
  accept_x(getS().x3)

  s.x = x
  s.x2 = x
  s.x3 = x // expected-error{{cannot assign to read-only variable or subscript}}

  getS().x = x // expected-error{{cannot assign to read-only variable or subscript}}
  getS().x2 = x // expected-error{{cannot assign to read-only variable or subscript}}
  getS().x3 = x // expected-error{{cannot assign to read-only variable or subscript}}

  accept_x_inout(&getS().x) // expected-error{{expression does not type-check}}
  accept_x_inout(&getS().x2) // expected-error{{expression does not type-check}}
  accept_x_inout(&getS().x3) // expected-error{{expression does not type-check}}

  x = getS().x
  x = getS().x2
  x = getS().x3

  accept_x_inout(&s.x)
  accept_x_inout(&s.x2)
  accept_x_inout(&s.x3) // expected-error{{expression does not type-check}}
}

extension S {
  def test(other_x: X) {
    x = other_x
    x2 = other_x
    x3 = other_x // expected-error{{cannot assign to read-only variable or subscript}}

    other_x = x
    other_x = x2
    other_x = x3
  }
}

// Accessor on non-settable type

struct Aleph {
  var b:Beth { get: return Beth(1) }
}

struct Beth {
  var c:Int
}

def accept_int_inout(c: @inout Int) { }
def accept_int(c: Int) { }

def test_settable_of_nonsettable(a: Aleph) {
  a.b.c = 1 // expected-error{{cannot assign to read-only variable or subscript}}
  var x:Int = a.b.c

  accept_int(a.b.c) // expected-FIXME-error {{not settable}}
  accept_int_inout(&a.b.c) // expected-error {{expression does not type-check}}
}

// Properties with initial values are not permitted; the '{' is actually a separate
// brace statement
// FIXME: QoI could be much better here.
var propWithInit : Int = 17 { // expected-error{{expression does not type-check}}
get: // expected-error{{expected expression}} expected-error{{consecutive statements}}
  return 42
}
