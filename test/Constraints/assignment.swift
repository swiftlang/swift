// RUN: %swift -parse -verify %s

struct X { }
struct Y { }

struct WithOverloadedSubscript {
  subscript(i: Int) -> X {
    get {}
    set {}
  }
  subscript(i: Int) -> Y {
    get {}
    set {}
  }
}

func test_assign() {
  var a = WithOverloadedSubscript()
  a[0] = X()
  a[0] = Y()
}

var i: X
var j: X
var f: Y
func getXY() -> (X, Y) {}
var ift : (X, Y)
var ovl = WithOverloadedSubscript()

var slice: X[]

i = j
(i, f) = getXY()
(i, f) = ift
(i, f) = (i, f)
(ovl[0], ovl[0]) = ift
(ovl[0], ovl[0]) = (i, f)
(_, ovl[0]) = (i, f)
(ovl[0], _) = (i, f)
_ = (i, f)
slice[7] = i

slice[7] = f // expected-error{{could not find an overload for 'subscript' that accepts the supplied arguments}}

slice[7] = _ // expected-error{{'_' can only appear in a pattern or on the left side of an assignment}}

func value(x: Int) {}
func inout(inout x: Int) {}
inout(&_) // expected-error{{'_' can only appear in a pattern or on the left side of an assignment}}
value(_) // expected-error{{'_' can only appear in a pattern or on the left side of an assignment}}

