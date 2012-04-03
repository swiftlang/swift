// RUN: %swift -I %S/.. %s -verify

struct X { }
struct Y { }
struct Z { }

func f0(x1 : X, x2 : X) -> X {} // expected-note{{found this candidate}}
func f0(y1 : Y, y2 : Y) -> Y {} // expected-note{{found this candidate}}
var f0 : X; // expected-note{{found this candidate}}
var f0 : (x : X, y : Y) -> X; // expected-note{{found this candidate}}

func test_conv() {
  var a1 : (x1 : X, x2 : X) -> X = f0;
  // FIXME: missing conversion for function types (!)
  // var a2 : (X, X) -> X = f0;
  var a3 : X = f0;
  var a4 : (x : X, y : Y) -> X = f0;
  var a5 : (Y, X) -> X = f0; // expected-error{{no candidates found for reference to overloaded 'f0'}} expected-note{{while converting 'var' initializer}}
}
