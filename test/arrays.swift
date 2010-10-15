//===----------------------------------------------------------------------===//
// Standard prolog library.
//===----------------------------------------------------------------------===//

// void is just a type alias for the empty tuple.
typealias void : ()
// int is just a type alias for the 32-bit integer type.
typealias int : __builtin_int32_type

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
var [infix=120] <  : (lhs : int, rhs : int) -> int
var [infix=120] >  : (lhs : int, rhs : int) -> int
var [infix=120] <= : (lhs : int, rhs : int) -> int
var [infix=120] >= : (lhs : int, rhs : int) -> int
var [infix=120] == : (lhs : int, rhs : int) -> int
var [infix=120] != : (lhs : int, rhs : int) -> int

func [infix=100] min(lhs : int, rhs : int) -> int;
func [infix=100] max(lhs : int, rhs : int) -> int;

// Basic Control flow.
func if (cond : int) -> (body : () -> ()) -> ();
func while (cond : int) -> (body : () -> ()) -> ();

data else_type {}

// FIXME: Replace ife with 'if' when we support overloading.
func [infix=250] else(iftrue : ()->(), iffalse : ()->()) -> else_type;
func ife(cond : int) -> else_type -> ();


//===----------------------------------------------------------------------===//
// Tests for arrays.
//===----------------------------------------------------------------------===//


func test1() {
  // Declare an array a "on the stack".
  var a : int[4]; 
  // sizeof(a) == 4*sizeof(int) bytes.


  // Operations work as expected.
  a[0] = 1;
  var b = a[1];
  a[4] = 1;  // fails in a currently unspecified way.

  // eventually support things like a[1..3] etc for slices once we have ranges.
  // Slices will always return an unsized array, not a sized one.
  // "typeof(a[1..3]) == int[]".


  // C is a reference to a (possible slice of) another array.
  var c : int[] = a;
  // sizeof(c) == two words: pointer + size
  c[0] = 1;
  c[4] = 1;  // fails in a currently unspecified way.

  // Arrays can be copied by value for consistency, though this isn't a common thing:
  var d : int[4] = a;

  // f1 takes array "by reference", and does what you expect.
  func f1(x : int[]);
  f1(a)

  // f2 takes an array by copy, should probably be "warning: use int[] for arguments to functions".
  func f2(x : int[4]);
  f2(a)

  // Two (and more) dimensional arrays should work in the expected way:
  var e : int[4][4];
  var f : int[4][] = e;
  f[1][2] = 4;

  func f3(x : int[4][]);
  f3(f); f3(e);

  // You can even have arrays of tuples and other things, that work right through composition:
  var array_of_tuples : (a : int, b : int)[42];
  var tuple_of_arrays : (a : int[42], b : int[42]);

  array_of_tuples[12].a = array_of_tuples[13].b;
  tuple_of_arrays.a[12] = array_of_tuples.b[13];

  // Array of function pointers, good luck declaring this in C :)
  var array_fn_ptrs : (int -> int)[42];
  var g = array_fn_ptrs[12](4);

  // Without parens, this is a function that returns a fixed size array:
  var fn_returning_array : int -> int[42];
  var h : int[42] = fn_returning_array(4);
}