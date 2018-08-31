// RUN: %target-typecheck-verify-swift

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

var slice: [X]

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

slice[7] = f // expected-error{{cannot assign value of type 'Y' to type 'X'}}

slice[7] = _ // expected-error{{'_' can only appear in a pattern or on the left side of an assignment}}

func value(_ x: Int) {}
func value2(_ x: inout Int) {}
value2(&_) // expected-error{{'_' can only appear in a pattern or on the left side of an assignment}}
value(_) // expected-error{{'_' can only appear in a pattern or on the left side of an assignment}}


// <rdar://problem/23798944> = vs. == in Swift if string character count statement causes segmentation fault
func f23798944() {
  let s = ""
  if s.count = 0 { // expected-error {{use of '=' in a boolean context, did you mean '=='?}}
  }
}

.sr_3506 = 0 // expected-error {{reference to member 'sr_3506' cannot be resolved without a contextual type}}
