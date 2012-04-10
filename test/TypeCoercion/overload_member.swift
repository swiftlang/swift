// RUN: %swift -I %S/.. %s -verify

struct X { }
struct Y { }
struct Z { }

struct A {
  func f(x : X) -> X { } // expected-note{{found this candidate}}
  func f(y : Y) -> Y { } // expected-note{{found this candidate}}

  func g(z : Z) -> X { }
  func g(z : Z) -> Y { }
}

func test_method_overload(a : A, x : X, y : Y) {
  var x1 = a.f(x)
  x1 = x

  var y1 = a.f(y)
  y1 = y
}

func test_method_overload_coerce(a : A, x : X, y : Y, z : Z) {
  var fail = a.g(z) // expected-error{{ambiguous expression was not resolved to a concrete type}}
  x = a.g(z)
  y = a.g(z)
}

func test_method_value_coerce(a : A) {
  var fp1 : (X) -> X = a.f;
  var fp2 : ([byref(implicit)] A) -> (X) -> X = A.f;
  var fp3 : ([byref] A) -> (X) -> X = A.f; // expected-error{{no candidates found for reference to overloaded 'f'}} expected-note{{while converting}}
}
