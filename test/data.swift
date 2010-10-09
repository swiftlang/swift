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



//===----------------------------------------------------------------------===//
// Tests for various simple data constructs
//===----------------------------------------------------------------------===//


