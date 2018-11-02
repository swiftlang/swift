// RUN: %target-typecheck-verify-swift

var a : Int

func test() {
  var y : a   // expected-error {{use of undeclared type 'a'}}
  var z : y   // expected-error {{use of undeclared type 'y'}}
  var w : Swift.print   // expected-error {{no type named 'print' in module 'Swift'}}
}

var b : (Int) -> Int = { $0 }

var c2 : (field : Int)  // expected-error {{cannot create a single-element tuple with an element label}}{{11-19=}}

var d2 : () -> Int = { 4 }

var d3 : () -> Float = { 4 }

var d4 : () -> Int = { d2 }  // expected-error{{function produces expected type 'Int'; did you mean to call it with '()'?}} {{26-26=()}}

var e0 : [Int]
e0[] // expected-error {{cannot subscript a value of type '[Int]' with an index of type '()'}}
  // expected-note @-1 {{overloads for 'subscript' exist with these partially matching parameter lists: (Int), (Range<Int>),}}

var f0 : [Float]
var f1 : [(Int,Int)]

var g : Swift // expected-error {{use of undeclared type 'Swift'}} expected-note {{cannot use module 'Swift' as a type}}

var h0 : Int?
_ = h0 == nil // no-warning
var h1 : Int??
_ = h1! == nil // no-warning
var h2 : [Int?]
var h3 : [Int]?
var h3a : [[Int?]]
var h3b : [Int?]?
var h4 : ([Int])?
var h5 : ([([[Int??]])?])?
var h7 : (Int,Int)?
var h8 : ((Int) -> Int)?
var h9 : (Int?) -> Int?
var h10 : Int?.Type?.Type

var i = Int?(42)

func testInvalidUseOfParameterAttr() {
  var bad_io : (Int) -> (inout Int, Int)  // expected-error {{'inout' may only be used on parameters}}
  func bad_io2(_ a: (inout Int, Int)) {}    // expected-error {{'inout' may only be used on parameters}}
  
  var bad_is : (Int) -> (__shared Int, Int)  // expected-error {{'__shared' may only be used on parameters}}
  func bad_is2(_ a: (__shared Int, Int)) {}    // expected-error {{'__shared' may only be used on parameters}}
  
  var bad_iow : (Int) -> (__owned Int, Int)  // expected-error {{'__owned' may only be used on parameters}}
  func bad_iow2(_ a: (__owned Int, Int)) {}  // expected-error {{'__owned' may only be used on parameters}}
}

// <rdar://problem/15588967> Array type sugar default construction syntax doesn't work
func test_array_construct<T>(_: T) {
  _ = [T]()   // 'T' is a local name
  _ = [Int]()  // 'Int is a global name'
  _ = [UnsafeMutablePointer<Int>]()  // UnsafeMutablePointer<Int> is a specialized name.
  _ = [UnsafeMutablePointer<Int?>]()  // Nesting.
  _ = [([UnsafeMutablePointer<Int>])]()
  _ = [(String, Float)]()
}

extension Optional {
  init() {
    self = .none
  }
}

// <rdar://problem/15295763> default constructing an optional fails to typecheck
func test_optional_construct<T>(_: T) {
  _ = T?()    // Local name.
  _ = Int?()  // Global name
  _ = (Int?)() // Parenthesized name.
}

// Test disambiguation of generic parameter lists in expression context.

struct Gen<T> {}

var y0 : Gen<Int?>
var y1 : Gen<Int??>
var y2 : Gen<[Int?]>
var y3 : Gen<[Int]?>
var y3a : Gen<[[Int?]]>
var y3b : Gen<[Int?]?>
var y4 : Gen<([Int])?>
var y5 : Gen<([([[Int??]])?])?>
var y7 : Gen<(Int,Int)?>
var y8 : Gen<((Int) -> Int)?>
var y8a : Gen<[([Int]?) -> Int]>
var y9 : Gen<(Int?) -> Int?>
var y10 : Gen<Int?.Type?.Type>
var y11 : Gen<Gen<Int>?>
var y12 : Gen<Gen<Int>?>?
var y13 : Gen<Gen<Int?>?>?
var y14 : Gen<Gen<Int?>>?
var y15 : Gen<Gen<Gen<Int?>>?>
var y16 : Gen<Gen<Gen<Int?>?>>
var y17 : Gen<Gen<Gen<Int?>?>>?

var z0 = Gen<Int?>()
var z1 = Gen<Int??>()
var z2 = Gen<[Int?]>()
var z3 = Gen<[Int]?>()
var z3a = Gen<[[Int?]]>()
var z3b = Gen<[Int?]?>()
var z4 = Gen<([Int])?>()
var z5 = Gen<([([[Int??]])?])?>()
var z7 = Gen<(Int,Int)?>()
var z8 = Gen<((Int) -> Int)?>()
var z8a = Gen<[([Int]?) -> Int]>()
var z9 = Gen<(Int?) -> Int?>()
var z10 = Gen<Int?.Type?.Type>()
var z11 = Gen<Gen<Int>?>()
var z12 = Gen<Gen<Int>?>?()
var z13 = Gen<Gen<Int?>?>?()
var z14 = Gen<Gen<Int?>>?()
var z15 = Gen<Gen<Gen<Int?>>?>()
var z16 = Gen<Gen<Gen<Int?>?>>()
var z17 = Gen<Gen<Gen<Int?>?>>?()

y0  = z0 
y1  = z1 
y2  = z2 
y3  = z3 
y3a = z3a
y3b = z3b
y4  = z4 
y5  = z5 
y7  = z7 
y8  = z8 
y8a = z8a
y9  = z9 
y10 = z10
y11 = z11
y12 = z12
y13 = z13
y14 = z14
y15 = z15
y16 = z16
y17 = z17


// Type repr formation.
// <rdar://problem/20075582> Swift does not support short form of dictionaries with tuples (not forming TypeExpr)
let tupleTypeWithNames = (age:Int, count:Int)(4, 5)
let dictWithTuple = [String: (age:Int, count:Int)]()

// <rdar://problem/21684837> typeexpr not being formed for postfix !
let bb2 = [Int!](repeating: nil, count: 2) // expected-warning {{using '!' is not allowed here; treating this as '?' instead}}{{15-16=?}}

// <rdar://problem/21560309> inout allowed on function return type
func r21560309<U>(_ body: (_: inout Int) -> inout U) {}  // expected-error {{'inout' may only be used on parameters}}
r21560309 { x in x }

// <rdar://problem/21949448> Accepts-invalid: 'inout' shouldn't be allowed on stored properties
class r21949448 {
  var myArray: inout [Int] = []   // expected-error {{'inout' may only be used on parameters}}
}

// SE-0066 - Standardize function type argument syntax to require parentheses
let _ : Int -> Float // expected-error {{single argument function types require parentheses}} {{9-9=(}} {{12-12=)}}
let _ : inout Int -> Float // expected-error {{'inout' may only be used on parameters}}
// expected-error@-1 {{single argument function types require parentheses}} {{15-15=(}} {{18-18=)}}
func testNoParenFunction(x: Int -> Float) {} // expected-error {{single argument function types require parentheses}} {{29-29=(}} {{32-32=)}}
func testNoParenFunction(x: inout Int -> Float) {} // expected-error {{single argument function types require parentheses}} {{35-35=(}} {{38-38=)}}

func foo1(a : UnsafePointer<Void>) {} // expected-warning {{UnsafePointer<Void> has been replaced by UnsafeRawPointer}}{{15-34=UnsafeRawPointer}}
func foo2(a : UnsafeMutablePointer<()>) {} // expected-warning {{UnsafeMutablePointer<Void> has been replaced by UnsafeMutableRawPointer}}{{15-39=UnsafeMutableRawPointer}}
class C {
  func foo1(a : UnsafePointer<Void>) {} // expected-warning {{UnsafePointer<Void> has been replaced by UnsafeRawPointer}}{{17-36=UnsafeRawPointer}}
  func foo2(a : UnsafeMutablePointer<()>) {} // expected-warning {{UnsafeMutablePointer<Void> has been replaced by UnsafeMutableRawPointer}}{{17-41=UnsafeMutableRawPointer}}
  func foo3() {
    let _ : UnsafePointer<Void> // expected-warning {{UnsafePointer<Void> has been replaced by UnsafeRawPointer}}{{13-32=UnsafeRawPointer}}
    let _ : UnsafeMutablePointer<Void> // expected-warning {{UnsafeMutablePointer<Void> has been replaced by UnsafeMutableRawPointer}}{{13-39=UnsafeMutableRawPointer}}
  }
}

let _ : inout @convention(c) (Int) -> Int // expected-error {{'inout' may only be used on parameters}}
func foo3(inout a: Int -> Void) {} // expected-error {{'inout' before a parameter name is not allowed, place it before the parameter type instead}} {{11-16=}} {{20-20=inout }}
                                   // expected-error @-1 {{single argument function types require parentheses}} {{20-20=(}} {{23-23=)}}

func sr5505(arg: Int) -> String {
  return "hello"
}
var _: sr5505 = sr5505 // expected-error {{use of undeclared type 'sr5505'}}

typealias A = (inout Int ..., Int ... = [42, 12]) -> Void // expected-error {{'inout' must not be used on variadic parameters}}
                                                          // expected-error@-1 {{only a single element can be variadic}} {{35-39=}}
                                                          // expected-error@-2 {{default argument not permitted in a tuple type}} {{39-49=}}
