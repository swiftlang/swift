// RUN: %target-typecheck-verify-swift

struct X { }
struct Y { }
struct Z { }

func f0(_ x1: X, x2: X) -> X {} // expected-note{{found this candidate}}
func f0(_ y1: Y, y2: Y) -> Y {} // expected-note{{found this candidate}}
var f0 : X // expected-note {{found this candidate}} expected-note {{'f0' previously declared here}}
func f0_init(_ x: X, y: Y) -> X {}
var f0 : (_ x : X, _ y : Y) -> X = f0_init // expected-error{{invalid redeclaration}}
func f1(_ x: X) -> X {}

func f2(_ g: (_ x: X) -> X) -> ((_ y: Y) -> Y) { }

func test_conv() {
  var _ : (_ x1 : X, _ x2 : X) -> X = f0
  var _ : (X, X) -> X = f0
  var _ : (Y, X) -> X = f0 // expected-error{{ambiguous reference to member 'f0(_:x2:)'}}
  var _ : (X) -> X = f1
  var a7 : (X) -> (X) = f1
  var a8 : (_ x2 : X) -> (X) = f1
  var a9 : (_ x2 : X) -> ((X)) = f1
  a7 = a8
  a8 = a9
  a9 = a7

  var _ : ((X) -> X) -> ((Y) -> Y) = f2
  var _ : ((_ x2 : X) -> (X)) -> (((_ y2 : Y) -> (Y))) = f2

  typealias fp = ((X) -> X) -> ((Y) -> Y)
  var _ = f2
}



var xy : X // expected-note {{previously declared here}}
var xy : Y // expected-error {{invalid redeclaration of 'xy'}}

func accept_X(_ x: inout X) { }
func accept_XY(_ x: inout X) -> X { }
func accept_XY(_ y: inout Y) -> Y { }
func accept_Z(_ z: inout Z) -> Z { }

func test_inout() {
  var x : X
  accept_X(&x);
  accept_X(xy); // expected-error{{passing value of type 'X' to an inout parameter requires explicit '&'}} {{12-12=&}}
  accept_X(&xy);

  _ = accept_XY(&x);
  x = accept_XY(&xy);

  x = xy
  x = &xy; // expected-error{{'&' used with non-inout argument of type 'X'}}
  accept_Z(&xy); // expected-error{{cannot convert value of type 'X' to expected argument type 'Z'}}
}

func lvalue_or_rvalue(_ x: inout X) -> X { }
func lvalue_or_rvalue(_ x: X) -> Y { }

func test_lvalue_or_rvalue() {
  var x : X
  var y : Y
  let x1 = lvalue_or_rvalue(&x)
  x = x1
  let y1 = lvalue_or_rvalue(x)
  y = y1
  _ = y
}
