// RUN: %swift %s -verify

var func4 : (fn : @auto_closure () -> ()) -> ()
var func6 : (fn : (Int,Int) -> Int) -> ()


// Expressions can be auto-closurified, so that they can be evaluated separately
// from their definition.
var closure1 : @auto_closure () -> Int = 4  // Function producing 4 whenever it is called.
var closure2 : (Int,Int) -> Int = { 4 } // FIXME: expected-error{{tuple types '(Int, Int)' and '()' have a different number of elements (2 vs. 0)}}
var closure3a : ()->()->(Int,Int) = {{ (4, 2) }} // multi-level closing.
var closure3b : (Int,Int)->(Int)->(Int,Int) = {{ (4, 2) }} // FIXME: expected-error{{different number of elements}}
var closure4 : (Int,Int) -> Int = { $0 + $1 }
var closure5 : (Double) -> Int =
   { // expected-error{{expression does not type-check}}
       $0 + 1.0 }

var closure6 = $0  // expected-error {{anonymous closure argument not contained in a closure}}

var closure7 : Int =
   { 4 }  // expected-error {{'() -> $T0' is not convertible to 'Int'}}

def funcdecl1(a: Int, y: Int) {}
def funcdecl3() -> Int {}
def funcdecl4(a: ((Int)->Int), b: Int) {} // expected-note{{in initialization of parameter 'a'}}

def funcdecl5(a: Int, y: Int) {
  // Pass in a closure containing the call to funcdecl3.
  funcdecl4({ funcdecl3() }, 12) // FIXME: expected-error{{'(Int)' is not a subtype of '()'}}
  func6({$0 + $1})       // Closure with two named anonymous arguments
  func6({($0) + $1})    // Closure with sequence expr inferred type
  func6({($0) + $0})    // FIXME: expected-error{{expression does not type-check}}


  var testfunc : ((), Int) -> Int
  testfunc(          
           {$0+1})  // expected-error {{'($0: $T2) -> $T1' is not convertible to '((), Int)'}}

  funcdecl5(1, 2) // recursion.

  // Element access from a tuple.
  var a : (Int, f : Int, Int)
  var b = a.1+a.f

  // Tuple expressions with named elements.
  var i : (y : Int, x : Int) = (x : 42, y : 11)
  funcdecl1(y : 123, a : 444)
  
  // Calls.
  4()  // expected-error {{expression does not type-check}}
  
  
  // rdar://12017658 - Infer some argument types from func6.
  func6({ a, b -> Int in a+b})
  // Return type inference.
  func6({ a,b in a+b })
  
  // Infer incompatible type.
  // FIXME: Need to relate diagnostic to return type
  func6({a,b->Float in 4.0 })    // expected-error {{'Float' is not a subtype of 'Int'}}

  // Pattern doesn't need to name arguments.
  func6({ _,_ in 4 })
  
  var fn = {} // FIXME: maybe? expected-error{{expression does not type-check}}
  var fn2 = { 4 }
  
  
  var c : Int = { a,b-> Int in a+b} // expected-error{{'(a: $T0, b: $T1) -> Int' is not convertible to 'Int'}}
  
  
}

// rdar://11935352 - closure with no body.
def closure_no_body(p: () -> ()) {
  return closure_no_body({})
}


// rdar://12019415
def t() {
  var u8 : StringByte
  var x : Bool

  if (0xA0..0xBF).contains(Int(u8)) && x {
  }
}

// <rdar://problem/11927184>
def f0(a: Any) -> Int { return 1 }
assert(f0(1) == 1)


var selfRef = { selfRef() } // expected-error {{variable used within its own initial value}}
var nestedSelfRef = { // expected-error {{expression does not type-check}}
  var recursive = { nestedSelfRef() } // expected-error {{variable used within its own initial value}}
  recursive()
}
