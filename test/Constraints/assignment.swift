// RUN: %target-typecheck-verify-swift

struct X { }
struct Y { }
protocol Z { }

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

struct WithProtocolSubscript {
  subscript(i: Int) -> Z {
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
var ps = WithProtocolSubscript()

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

slice[7] = f // expected-error{{cannot assign value of type 'Y' to subscript of type 'X'}}

slice[7] = nil // expected-error{{'nil' cannot be assigned to subscript of type 'X'}}

ps[7] = i // expected-error{{cannot assign value of type 'X' to subscript of type 'any Z'}}

slice[7] = _ // expected-error{{'_' can only appear in a pattern or on the left side of an assignment}}

func value(_ x: Int) {}
func value2(_ x: inout Int) {}
value2(&_) // expected-error{{'_' can only appear in a pattern or on the left side of an assignment}}
value(_) // expected-error{{'_' can only appear in a pattern or on the left side of an assignment}}


// <rdar://problem/23798944> = vs. == in Swift if string character count statement causes segmentation fault
func f23798944() {
  let s = ""
  if s.count = 0 { // expected-error {{use of '=' in a boolean context, did you mean '=='?}}
    // expected-error@-1{{cannot assign to property: 'count' is a get-only property}}
  }
}

// https://github.com/apple/swift/issues/46094
do {
  .x = 0 // expected-error {{type 'Int' has no member 'x'}}
}

// https://github.com/apple/swift/issues/44162
func returnsVoid() {}
_ = returnsVoid() // expected-warning {{using '_' to ignore the result of a Void-returning function is redundant}}{{1-5=}}

// https://github.com/apple/swift/issues/56396
class С_56396 {
  var callback: ((С_56396) -> Void)!
  
  func setCallback(_ callback: @escaping (Self) -> Void) {
    self.callback = callback // expected-error {{cannot assign value of type '(Self) -> Void' to type '(С_56396) -> Void'}}
  }
}

// https://github.com/swiftlang/swift/issues/82397
func testFunctionAssignsWithOptionals(fn: @escaping () -> () -> Void) {
  let _: (() -> () -> Void)? = fn
  let _: (() -> () -> Void)?? = fn

  class Super {}
  class Sub: Super {}

  let b: () -> () -> Sub = { { return Sub() } }
  let _: (() -> () -> Super)? = b
  let _: (() -> () -> Super)?? = b
}

final class NestedAssignmentA {
  var x: Int?
}

func testNestedAssignmentWithOptionalChaining(a: NestedAssignmentA?) {
  a?.x = a?.x = 1 // expected-error {{cannot assign value of type '()' to type 'Int'}}
}

final class NestedAssignmentC {}

final class NestedAssignmentB {
  var x: NestedAssignmentC?
}

final class NestedAssignmentContainer {
  var a: NestedAssignmentB?
}

func testNestedAssignmentThroughMemberChain(
    b: NestedAssignmentContainer,
    c: NestedAssignmentC
) {
  b.a?.x = b.a?.x = c // expected-error {{cannot assign value of type '()' to type 'NestedAssignmentC'}}
}

struct NestedAssignmentS {
  var x: Int?
}

func testNestedAssignmentWithInOutOptional(
    s: inout NestedAssignmentS?
) {
  s?.x = s?.x = 1 // expected-error {{cannot assign value of type '()' to type 'Int'}}
}

final class NestedAssignmentParenA {}
final class NestedAssignmentParenB {
  var x: NestedAssignmentParenA?
}
final class NestedAssignmentParenContainer {
  var a: NestedAssignmentParenB?
}
func testNestedAssignmentWithParenthesizedInnerAssign(
    b: NestedAssignmentParenContainer,
    c: NestedAssignmentParenA
) {
  b.a?.x = (b.a?.x = c) // expected-error {{cannot assign value of type '()' to type 'NestedAssignmentParenA'}}
}

final class NestedAssignmentLegalA {
  var x: Int?
}
func testOptionalChainedAssignmentWithOptionalRHSIsLegal(
    a: NestedAssignmentLegalA?
) {
  a?.x = a?.x
}
