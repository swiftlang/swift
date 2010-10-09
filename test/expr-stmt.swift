//===----------------------------------------------------------------------===//
// Standard prolog library.
//===----------------------------------------------------------------------===//

// void is just a type alias for the empty tuple.
typealias void : ();
// int is just a type alias for the 32-bit integer type.
typealias int : __builtin_int32_type;

// Simple arithmetic operators.
func [infix=150] * (lhs: int, rhs: int) -> int;
func [infix=150] / (lhs: int, rhs: int) -> int;
func [infix=150] % (lhs: int, rhs: int) -> int;
func [infix=120] + (lhs: int, rhs: int) -> int;
func [infix=120] - (lhs: int, rhs: int) -> int;

// Short circuiting logical operators.
// Implement in terms of 'if'.
func [infix=100] || (lhs: int, rhs: ()->int) -> int;
func [infix=100] && (lhs: int, rhs: ()->int) -> int;

// TODO: Give a correct precedence (match C).
var [infix=120] <  : (int, int) -> int;
var [infix=120] >  : (int, int) -> int;
var [infix=120] <= : (int, int) -> int;
var [infix=120] >= : (int, int) -> int;
var [infix=120] == : (int, int) -> int;
var [infix=120] != : (int, int) -> int;
func [infix=100] min(lhs : int, rhs : int) -> int;
func [infix=100] max(lhs : int, rhs : int) -> int;

// Basic Control flow.
func if (cond : int) -> (body : () -> ()) -> ();
func while (cond : int) -> (body : () -> ()) -> ();


// FIXME: Replace __builtin_else_hack_type with a user defined type eventually.
typealias else_type : __builtin_else_hack_type;

// FIXME: Replace ife with 'if' when we support overloading.
func [infix=250] else(iftrue : ()->(), iffalse : ()->()) -> else_type;
func ife(cond : int) -> else_type -> ();


// Can define "unless" and "until" from bcpl.  Can define a postfix form of
// unless as in: foo() unless (cond).

//===----------------------------------------------------------------------===//
// Tests and samples.
//===----------------------------------------------------------------------===//

// Comment.

// Simple integer variables.
var x : int;
var x2 = 4;              // Simple Type inference.
var x3 = 4+x*(4+x2)/97;  // Basic Expressions.

// Declaring a variable void, aka (), is fine too.
var v : void;

//var x4 : float = 4+5;


// Various tuple types.
var tuple1 : ();
var tuple2 : (int);
var tuple3 : (int, int, ());
var tuple2a : (.a : int);
var tuple3a : (.a : int, .b : int, .c : ());

var tuple4 = (1, 2);        // Tuple literal.
var tuple5 = (1, 2, 3, 4);  // Tuple literal.
var tuple6 = (1 2 3 4);     // Grouping paren around sequence, w/dead exprs.

// Various function types.
var func1 : () -> ();      // No input, no output.
var func1a : void -> void;  // same as func1
var func2 : (int) -> int;
var func3 : () -> () -> ();     // Takes nothing, returns a function.
var func3a : () -> (() -> ());  // same as func3
var func4 : (() -> ()) -> ();   // Takes a function, returns nothing.
var func5 : ((int,int) -> ()) -> ();   // Takes a function, returns nothing.
var func6 : ((int,int) -> int) -> ();  // Takes a function, returns nothing.

// Brace expressions.
var brace1 = { 4; 5; };
var brace2 = { 4; 5+7 };
var brace3 = {
  var brace2 = 42;  // variable shadowing.
  brace2+7
};

// Function calls.
var call1 = func1();
var call2 = func2(1);
var call3 = func2 1;  // Grouping parens aren't needed.
var call4 = func3()();


// Infix operators and attribute lists.
var [infix=2] fooinfix : (int, int) -> ();
var [] infixtest = 4 % 2 + 27 fooinfix 123;

// Expressions can be auto-closurified, so that they can be evaluated separately
// from their definition.
var closure1 : () -> int = 4;  // Function producing 4 whenever it is called.
var closure2 : (int,int) -> int = 4; // Has some (dead) arguments.
var closure3 : (int,int)->int->(int,int) = (4, 2); // multi-level closing.
var closure4 : (int,int) -> int = $0 + $1;


// The func keyword gives a nice simplification for function definitions.
func funcdecl1(a : int, y : int) {}
func funcdecl2() funcdecl1(4, 2)  // Arbitrary exprs can be used as a body.
func funcdecl3() -> int = 12;  // Optional equal sign, optional ;.
func funcdecl4(a : (int->int), b : int);
func signal(sig : int, f : (int)->void) -> (:int)->void;

func funcdecl5(a : int, y : int) {
  // a few "statements" from the standard prolog.
  if (x) {
    if (x || funcdecl3()) {
      while(1) { 4; 2 min 123 min 425; }
    }
  }

  // if/then/else.
  ife (x) {
  } else {
  }

  ife (x)
    funcdecl1(1,2)
  else
    funcdecl2();

  ife (x) {
    ife (x)
      funcdecl1(1,2)
    else
      funcdecl2();
  } else           // depends on precedence of else.
    funcdecl2();   // FIXME: Make else non-associative.

  // Pass in a closure containing the call to funcdecl3.
  funcdecl4(funcdecl3(), 12);
  func4(funcdecl2());
  func5 { funcdecl2(); }; // Closure with two implicit arguments
  func5 { $0 + $1; }      // Closure with two named anonymous arguments
  func6((4 $0) + $1);     // Closure with sequence expr inferred type
  func6({$0; 4} + $0);    // First $0 should get int type because of second.

  //var testfunc : (((),int)->int) -> int;
  //testfunc($0+1);   // Diagnose as an error.

  funcdecl5(1, 2); // recursion.

  // Element access from a tuple.
  var a : (int, .f : int, int);
  var b = a.field0+a.field1+a.f;

  // Richer name binding.
  var c = (4, 5);
  var (d, e) = (c.field1, c.field0);
  var ((), (g1, g2), h) = ((), (e, d), e);

  // Tuple expressions with named elements.
  var i : (.y : int, .x : int) = (.x = 42, .y = 11);
  funcdecl1(.y = 123, .a = 444);

  c.field1 = 4;
}

// Doing fun things with named arguments.  Basic stuff first.
func funcdecl6(a : int, b : int) -> int { a+b }

// Can dive into tuples, 'b' is a reference to the whole tuple, c and d are
// fields in it.  Cannot dive into functions or through aliases.
func funcdecl7(a : int, b : (.c : int, .d : int)) -> int {
  a + b.field0 + b.c + c + d
}

// TODO: Result can be named as well, but is writeonly.  Need to model lvalues
// and support the '=' operator.

// Type aliases.
typealias int_pair : (int,int);
typealias int_triple : (int,int, int);
typealias pair_Stuff : (int_pair, int_triple);
var ta_test : pair_Stuff = ((4,2), (1,2,3));



var test : int->int->int = $0;
func test2 (a : int) -> (b : int) -> (c : int) = $0;


func test3(arg1 : int, arg2 : int) -> int = 4;

func test4 () -> (arg1 : int, arg2 : int) -> int = test3;


// Functions can obviously take and return values.  Anonymous names are just
// types, they drop the 'var' in the argument list.
func w3(a : int) -> int = { a }
func w4( : int) -> int = { 4 }



func b1() {}

func foo1 (a : int, b: int) -> int;
func foo2 (a : int) -> (b: int) -> int;



// More realistic examples.

func fib(n : int) -> int {
  if (n < 2)
    {}  //return 1;  // Need Return!

  fib(n-2) + fib(n-1)
}

//func fib2(n : int) -> int {
//  if (n < 2)
//    ret 1;
//
//  ret fib(n-2) + fib(n-1);
//}




