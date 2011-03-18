// RUN: %swift %s -verify

//===----------------------------------------------------------------------===//
// Standard prolog library.
//===----------------------------------------------------------------------===//

// void is just a type alias for the empty tuple.
typealias void : ()
// int is just a type alias for the 32-bit integer type.
typealias int : __builtin_int32_type

typealias integer_literal_type : int

oneof bool {
false, true
}
var true : bool = bool::true
var false : bool = bool::false

// Simple arithmetic operators.
func [infix=200] * (lhs: int, rhs: int) -> int
func [infix=200] / (lhs: int, rhs: int) -> int
func [infix=200] % (lhs: int, rhs: int) -> int
func [infix=190] + (lhs: int, rhs: int) -> int
func [infix=190] - (lhs: int, rhs: int) -> int
// In C, 180 is <<, >>
var [infix=170] <  : (lhs : int, rhs : int) -> bool
var [infix=170] >  : (lhs : int, rhs : int) -> bool
var [infix=170] <= : (lhs : int, rhs : int) -> bool
var [infix=170] >= : (lhs : int, rhs : int) -> bool
var [infix=160] == : (lhs : int, rhs : int) -> bool
var [infix=160] != : (lhs : int, rhs : int) -> bool
// In C, 150 is &
// In C, 140 is ^
// In C, 130 is |

// Short circuiting logical operators.
// TODO: Implement in terms of 'if', once we have 'return'.
func [infix=120] && (lhs: bool, rhs: ()->bool) -> bool
func [infix=110] || (lhs: bool, rhs: ()->bool) -> bool
// In C, 100 is ?:
// In C, 90 is =, *=, += etc.



func [infix=100] min(lhs : int, rhs : int) -> int
func [infix=100] max(lhs : int, rhs : int) -> int

// Basic Control flow.
func if (cond : bool) -> (body : () -> ()) -> ()
func while (cond : bool) -> (body : () -> ()) -> ()

oneof else_type {}

// FIXME: Replace ife with 'if' when we support overloading.
func [infix=250] else(iftrue : ()->(), iffalse : ()->()) -> else_type;
func ife(cond : bool) -> else_type -> ();


// Can define "unless" and "until" from bcpl.  Can define a postfix form of
// unless as in: foo() unless (cond).


