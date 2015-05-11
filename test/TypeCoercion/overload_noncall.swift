// RUN: %target-parse-verify-swift

struct X { }
struct Y { }
struct Z { }

func f0(x1: X, x2: X) -> X {}
func f0(y1: Y, y2: Y) -> Y {}
var f0 : X // expected-note{{'f0' previously declared here}}
func f0_init(x: X, y: Y) -> X {}
var f0 : (x : X, y : Y) -> X = f0_init // expected-error{{invalid redeclaration}}
func f1(x: X) -> X {}

func f2(g: (x: X) -> X) -> ((y: Y) -> Y) { }

func test_conv() {
  var a1 : (x1 : X, x2 : X) -> X = f0;
  var a2 : (X, X) -> X = f0;
  var a5 : (Y, X) -> X = f0; // expected-error{{could not find an overload for 'f0' that accepts the supplied arguments}}
  var a6 : (X) -> X = f1;
  var a7 : (X) -> (X) = f1;
  var a8 : (x2 : X) -> (X) = f1;
  var a9 : (x2 : X) -> ((X)) = f1;
  a7 = a8;
  a8 = a9;
  a9 = a7;

  var a10 : ((X)->X) -> ((Y) -> Y) = f2;
  var a11 : ((x2 : X)-> (X)) -> (((y2 : Y) -> (Y))) = f2;

  typealias fp = ((X)->X) -> ((Y) -> Y)
  var a12 = f2
}



var xy : X // expected-note {{previously declared here}}
var xy : Y // expected-error {{invalid redeclaration of 'xy'}}

func accept_X(inout x: X) { }
func accept_XY(inout x: X) -> X { }
func accept_XY(inout y: Y) -> Y { }
func accept_Z(inout z: Z) -> Z { }

func test_inout() {
  var x : X;
  accept_X(&x);
  accept_X(xy); // expected-error{{passing value of type 'X' to an inout parameter requires explicit '&'}}
  accept_X(&xy);

  accept_XY(&x);
  x = accept_XY(&xy);

  x = xy;
  x = &xy; // expected-error{{cannot assign a value of type 'inout X' to a value of type 'X'}}
  accept_Z(&xy); // expected-error{{cannot invoke 'accept_Z' with an argument list of type '(inout X)'}} expected-note{{expected an argument list of type '(inout Z)'}}
}

func lvalue_or_rvalue(inout x: X) -> X { }
func lvalue_or_rvalue(x: X) -> Y { }

func test_lvalue_or_rvalue() {
  var x : X;
  var y : Y;
  let x1 = lvalue_or_rvalue(&x)
  x = x1
  let y1 = lvalue_or_rvalue(x)
  y = y1
  _ = y
}
