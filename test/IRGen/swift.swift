// RUN: %swift %s -verify

//===----------------------------------------------------------------------===//
// Swift Standard Prolog Library.
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Standard Types
//===----------------------------------------------------------------------===//


// void is just a type alias for the empty tuple.
typealias void : ()

// int is just a wrapper around the 64-bit integer type.
struct int { value : __builtin_int64_type }
// TODO: int8, uint8, int32, uint32, etc.

typealias integer_literal_type : int

// bool is the standard way to reason about truth values.
oneof bool {
  false, true
}
var true : bool = bool::true
var false : bool = bool::false

//===----------------------------------------------------------------------===//
// Logic Value Evalution For Control Flow
//===----------------------------------------------------------------------===//

// logic_value is the standard type to be used for values that can be used in
// control flow conditionals.
typealias logic_value : __builtin_int1_type

// logic_value's are always logic_values.
func convertToLogicValue(v : logic_value) -> logic_value {
  return v
}

// Bool can convert to a logic value (using matching eventually).
func convertToLogicValue(v : bool) -> logic_value




//===----------------------------------------------------------------------===//
// Standard Operators
//===----------------------------------------------------------------------===//

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
