// RUN: %swift %s -verify

import swift

//===----------------------------------------------------------------------===//
// Tests and samples.
//===----------------------------------------------------------------------===//

// Comment.

// Various function types.
var func1 : () -> ();                         // No input, no output.
var func1a : void -> void;                    // same as func1
var func2 : (:int) -> int;
var func3 : () -> () -> ();                   // Takes nothing, returns a fn.
var func3a : () -> (:() -> ());               // same as func3
var func4 : (fn : () -> ()) -> ();            // Takes a fn, returns nothing.
var func5 : (fn : (:int,:int) -> ()) -> ();   // Takes a fn, returns nothing.
var func6 : (fn : (:int,:int) -> int) -> ();  // Takes a fn, returns nothing.
var func7 : () -> (:int,:int,:int);           // Takes nothing, returns tuple.

// Top-Level expressions.  These are 'main' content.
func1();
4+7;

var bind_test1 : () -> () = func1;
var bind_test2 : int = 4 func1; // expected-error {{expression resolves to an unevaluated function}}

func basictest() {
  // Simple integer variables.
  var x : int
  var x2 = 4              // Simple Type inference.
  var x3 = 4+x*(4+x2)/97  // Basic Expressions.

  // Declaring a variable void, aka (), is fine too.
  var v : void

  var x4 : bool = true
  var x5 : bool =  // expected-note {{while converting 'var' initializer to declared type}}
        4 // expected-error {{invalid conversion from type 'integer_literal_type' to 'bool'}}

  //var x6 : float = 4+5;

  // FIXME: This should be rejected: the 5 is not bound.
  var x7 = 4 5


  // Various tuple types.
  var tuple1 : ();
  var tuple2 : (:int);
  var tuple3 : (:int, :int, : ());
  var tuple2a : (a : int);
  var tuple3a : (a : int, b : int, c : ());

  var tuple4 = (1, 2);        // Tuple literal.
  var tuple5 = (1, 2, 3, 4);  // Tuple literal.
  var tuple6 = (1 2 3 4);     // Grouping paren around sequence, w/dead exprs.


  // Brace expressions.
  var brace1 = ( 4 5 );
  var brace2 = ( 4 5+7 );
// FIXME: func() syntax.
//  var brace3 = {
//    var brace2 = 42;  // variable shadowing.
//    brace2+7
//  };

  // Function calls.
  var call1 = func1()
  var call2 = func2(1);
  var call3 = func2 1;  // Grouping parens aren't needed.
  var call4 = func3()();

  // Cannot call an integer.
  bind_test2(); // expected-error {{tuple expression isn't bound to identifier}}
}

// Infix operators and attribute lists.
var [infix=2] fooinfix : (: int, :int) -> ();
var [] infixtest : () = 4 % 2 + 27 fooinfix 123;

// Expressions can be auto-closurified, so that they can be evaluated separately
// from their definition.
var closure1 : () -> int = 4;  // Function producing 4 whenever it is called.
var closure2 : (:int,:int) -> int = 4; // Has some (dead) arguments.
var closure3 : (:int,:int)->int->(:int,:int) = (4, 2); // multi-level closing.
var closure4 : (:int,:int) -> int = $0 + $1;


// The func keyword gives a nice simplification for function definitions.
func funcdecl1(a : int, y : int) {}
func funcdecl2() {
  // return
  funcdecl1(4, 2)
}
func funcdecl3() -> int {
  //return
  12
}
func funcdecl4(a : (:int->int), b : int)
func signal(sig : int, f : (:int)->void) -> (:int)->void;

func funcdecl5(a : int, y : int) {
  // Pass in a closure containing the call to funcdecl3.
  funcdecl4(funcdecl3(), 12);
  func4(funcdecl2());
  func5(funcdecl2());     // Closure with two implicit arguments
  func5($0 + $1 ())          // Closure with two named anonymous arguments
  func6((4 $0) + $1);     // Closure with sequence expr inferred type
  func6(($0 4) + $0);    // First $0 should get int type because of second.

  var testfunc : (:(), :int) -> int;
  testfunc  // expected-note {{while converting function argument to expected type}}
    ($0+1);  // expected-error {{invalid conversion from type 'int' to '(: (), : int)'}}

  funcdecl5(1, 2); // recursion.

  // Element access from a tuple.
  var a : (:int, f : int, : int);
  var b = a.$1+a.f;

  // Tuple expressions with named elements.
  var i : (y : int, x : int) = (.x = 42, .y = 11);
  funcdecl1(.y = 123, .a = 444);
}

// Doing fun things with named arguments.  Basic stuff first.
func funcdecl6(a : int, b : int) -> int { a+b }

// Can dive into tuples, 'b' is a reference to the whole tuple, c and d are
// fields in it.  Cannot dive into functions or through aliases.
func funcdecl7(a : int, b : (c : int, d : int)) -> int {
  a + b.$0 + b.c + c + d
}

// Error recovery.
func testfunc2 (: (:(), :int) -> int) -> int;
func errorRecovery() {
  testfunc2 // expected-note {{while converting function argument to expected type}}
    ($0  // expected-error {{invalid conversion from type '()' to 'int'}}
      +1); // expected-note {{while converting left side of binary operator to expected type}}

  oneof oneof1 { bar, baz } // expected-note {{type declared here}}
  var a : int =  // expected-note {{while converting 'var' initializer to declared type}}
      :hello; // expected-error {{type 'int' has no member named 'hello'}}
  var b : oneof1 = :bar; // ok
  var c : oneof1 = // expected-note {{while converting 'var' initializer to declared type}}
      :xyz;  // expected-error {{type 'oneof1' has no member named 'xyz'}}
  var d : (:int,:int,:int) = // expected-note {{while converting 'var' initializer to declared type}}
      (1,2); // expected-error {{no value to initialize tuple element #2}}
  var e : (:int,:int) = // expected-note {{while converting 'var' initializer to declared type}}
      (1, 2, 3); // expected-error {{element #2 of tuple value not used when converting}}

  var f : (:int,:int) = // expected-note {{while converting 'var' initializer to declared type}}
      (1, 2, .f = 3); // expected-error {{tuple element 'f' (#2) of tuple value not used when converting}}
}




// TODO: Result can be named as well, but is writeonly.  Need to model lvalues
// and support the '=' operator.

// Type aliases.
typealias int_pair : (:int,:int);
typealias int_triple : (:int,:int, :int);
typealias pair_Stuff : (:int_pair, :int_triple);
var ta_test : pair_Stuff = ((4,2), (1,2,3));



var test : int->int->int = $0;
func test2 (a : int) -> (b : int) -> (c : int) {
  // FIXME: return $0
}


func test3(arg1 : int, arg2 : int) -> int {
  // return
  4
}

func test4 () -> (arg1 : int, arg2 : int) -> int {
  // FIXME: return test3
}

func test5() {
  var a : (:int, :int);
  var   // expected-note {{while converting 'var' initializer to declared type}}
     b : (: int->int, :int) = a;  // expected-error {{element #0 of tuple value has type 'int', but expected type 'int -> int'}}


  var c : (a : int, b : int);
  var d : (b : int, a : int) = c;  // Ok, reshuffle tuple.
}


// Functions can obviously take and return values.
func w3(a : int) -> int { a }
func w4( : int) -> int { 4 }



func b1() {}

func foo1(a : int, b: int) -> int;
func foo2(a : int) -> (b: int) -> int;
func foo3(a : int = 2, b : int = 3);


func test_lambda() {
  // A simple lambda.
  var a = lambda(val : int) { print(val+1) }

  // A recursive lambda.
  var fib = lambda(n : int) {
    if (n < 2) {} // return n;
    //return
    fib(n-1)+fib(n-2)
  }
}


// More realistic examples.

func fib(n : int) -> int {
  if (n < 2)
    {}  //return 1;  // Need Return!

  // return
  fib(n-2) + fib(n-1)
}

//func fib2(n : int) -> int {
//  if (n < 2)
//    return n
//
//  return fib(n-2) + fib(n-1)
//}




