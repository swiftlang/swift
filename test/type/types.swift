// RUN: %swift %s -verify

var a : Int

func test() {
  var y : a   // expected-error {{use of undeclared type 'a'}} expected-note {{here}}
  var z : y   // expected-error {{'y' is not a type}}
}

var b : Int -> Int = {$0}

@auto_closure var c1 : () -> Int  // expected-error {{attribute can only be applied to types, not declarations}}
var c2 : (field : @auto_closure Int)  // expected-error {{attribute only applies to syntactic function types}}
var c3 : (field : @auto_closure Int -> Int)  // expected-error {{auto_closure argument type must be '()'}}

var d1 : (field : @auto_closure () -> Int)
var d2 : @auto_closure () -> Int = 4

var d3 : @auto_closure () -> Float =
   4

var d4 : @auto_closure () -> Int =
   d2 // expected-error{{function produces expected type 'Int'; did you mean to call it with '()'?}}
   
var e0 : Int[]
e0[] // expected-error {{could not find an overload for 'subscript' that accepts the supplied arguments}}

var f0 : Float[]
var f1 : (Int,Int)[]

var g : Swift // expected-error {{use of module 'Swift' as a type}}

var h0 : Int?
!h0 // no-warning
var h1 : Int??
!h1! // no-warning
var h2 : Int?[]
var h3 : Int[]? // expected-error {{optional array type requires parentheses}} {{10-10=(}} {{15-15=)}}
var h4 : (Int[])?
var h5 : ((Int??[][])?[])?
var h6 : Int??[][]?[]? = h5 // expected-error {{optional array type requires parentheses}} {{10-10=(}} {{19-19=)}}  expected-error {{optional array type requires parentheses}} {{10-10=(}} {{22-22=)}}
var _ : Int = (h6![0]![0][0]!)! // no-warning
var h7 : (Int,Int)?
var h8 : (Int -> Int)?
var h9 : Int? -> Int?
var h10 : Int?.Type?.Type

var h6a : Int!![][]![]! // expected-error {{implicitly unwrapped optional array type requires parentheses}} {{11-11=(}} {{20-20=)}}  expected-error {{implicitly unwrapped optional array type requires parentheses}} {{11-11=(}} {{23-23=)}}

var i = Int?(42)

var j0 = new Int?[4]
var j1 = new Int.Type[4] {i in Int.self}
var j2 = new (Int -> Int)[4] {i in {$0}}
var j3 = new (Int[])[4]


var bad_io : (Int) -> (inout Int, Int)  // expected-error {{'inout' is only valid in parameter lists}}

func bad_io2(a: (inout Int, Int)) {}    // expected-error {{'inout' is only valid in parameter lists}}

// <rdar://problem/15588967> Array type sugar default construction syntax doesn't work
func test_array_construct<T>() {
  var a = T[]()   // 'T' is a local name
  var b = Int[]()  // 'Int is a global name'
  var c = UnsafePointer<Int>[]()  // UnsafePointer<Int> is a specialized name.
  var d = UnsafePointer<Int?>[]()  // Nesting.
  var e = (UnsafePointer<Int>[])[]()
}

// <rdar://problem/15295763> default constructing an optional fails to typecheck
func test_optional_construct<T>() {
  var a = T?()    // Local name.
  var b = Int?()  // Global name
  var c = (Int?)() // Parenthesized name.
}


