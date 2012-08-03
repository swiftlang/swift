// RUN: %swift %s -verify

var func4 : (fn : [auto_closure] () -> ()) -> ()
var func6 : (fn : (Int,Int) -> Int) -> ()    // Takes a fn, returns nothing.


// Expressions can be auto-closurified, so that they can be evaluated separately
// from their definition.
var closure1 : [auto_closure] () -> Int = 4  // Function producing 4 whenever it is called.
var closure2 : (Int,Int) -> Int = { 4 } // Has some (dead) arguments.
var closure3 : (Int,Int)->(Int)->(Int,Int) = {{ (4, 2) }} // multi-level closing.
var closure4 : (Int,Int) -> Int = { $0 + $1 }
var closure5 : (Double) -> Int =  // expected-note {{while converting}}
   {
       $0 + 1.0 }  // expected-error {{invalid conversion from type 'Double' to 'Int'}}

var closure6 = $0  // expected-error {{anonymous closure argument not contained in a closure}}

var closure7 : Int =  // expected-note {{while converting}}
   { 4 }  // expected-error {{closure inferred to have non-function type 'Int'}}

func funcdecl1(a : Int, y : Int)
func funcdecl3() -> Int
func funcdecl4(a : ((Int)->Int), b : Int)

func funcdecl5(a : Int, y : Int) {
  // Pass in a closure containing the call to funcdecl3.
  funcdecl4({ funcdecl3() }, 12)
  func6({$0 + $1})       // Closure with two named anonymous arguments
  func6({($0) + $1})    // Closure with sequence expr inferred type
  func6({($0) + $0})    // First $0 should get Int type because of second.


  var testfunc : ((), Int) -> Int
  testfunc(          // expected-note {{while converting}}
           {$0+1})  // expected-error {{closure inferred to have non-function type '((), Int)'}}

  funcdecl5(1, 2) // recursion.

  // Element access from a tuple.
  var a : (Int, f : Int, Int)
  var b = a.$1+a.f

  // Tuple expressions with named elements.
  var i : (y : Int, x : Int) = (x = 42, y = 11)
  funcdecl1(y = 123, a = 444)
  
  // Calls.
  4()  // expected-error {{called expression isn't a function}}
}

// rdar://11935352 - closure with no body.
func closure_no_body(p : () -> ()) {
  return closure_no_body({})
}

  //func6(func(a,b){a + b})  // Inferred argument type 'func' expression



// rdar://12019415
func t() {
  var u8 : StringByte
  var x : Bool

  if (0xA0 .. 0xBF).contains(u8) && x {
  }
}
