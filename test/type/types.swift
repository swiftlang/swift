// RUN: %swift %s -verify

var a : Int

func test() {
  var y : a   // expected-error {{use of undeclared type 'a'}} expected-note {{here}}
  var z : y   // expected-error {{'y' is not a type}}
}

var b : Int -> Int = {$0}

@autoclosure var c1 : () -> Int  // expected-error {{attribute can only be applied to types, not declarations}}
var c2 : (field : @autoclosure Int)  // expected-error {{attribute only applies to syntactic function types}}
var c3 : (field : @autoclosure Int -> Int)  // expected-error {{autoclosure argument type must be '()'}}

var d1 : (field : @autoclosure () -> Int)
var d2 : @autoclosure () -> Int = 4

var d3 : @autoclosure () -> Float =
   4

var d4 : @autoclosure () -> Int =
   d2 // expected-error{{function produces expected type 'Int'; did you mean to call it with '()'?}}

var e0 : [Int]
e0[] // expected-error {{missing argument for parameter #1 in call}}

var f0 : [Float]
var f1 : [(Int,Int)]

var g : Swift // expected-error {{use of module 'Swift' as a type}}

var h0 : Int?
h0 == nil // no-warning
var h1 : Int??
h1! == nil // no-warning
var h2 : [Int?]
var h3 : [Int]?
var h3a : [[Int?]]
var h3b : [Int?]?
var h4 : ([Int])?
var h5 : ([([[Int??]])?])?
var h7 : (Int,Int)?
var h8 : (Int -> Int)?
var h9 : Int? -> Int?
var h10 : Int?.Type?.Type

var i = Int?(42)

var bad_io : (Int) -> (inout Int, Int)  // expected-error {{'inout' is only valid in parameter lists}}

func bad_io2(a: (inout Int, Int)) {}    // expected-error {{'inout' is only valid in parameter lists}}

// <rdar://problem/15588967> Array type sugar default construction syntax doesn't work
func test_array_construct<T>() {
  var a = [T]()   // 'T' is a local name
  var b = [Int]()  // 'Int is a global name'
  var c = [UnsafeMutablePointer<Int>]()  // UnsafeMutablePointer<Int> is a specialized name.
  var d = [UnsafeMutablePointer<Int?>]()  // Nesting.
  var e = [([UnsafeMutablePointer<Int>])]()
}

// <rdar://problem/15295763> default constructing an optional fails to typecheck
func test_optional_construct<T>() {
  var a = T?()    // Local name.
  var b = Int?()  // Global name
  var c = (Int?)() // Parenthesized name.
}


