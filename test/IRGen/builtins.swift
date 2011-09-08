// RUN: %swift %s -emit-llvm -o - | FileCheck %s

typealias int : Builtin::int32
typealias bool : Builtin::int1

func [infix_left=200] * (lhs: int, rhs: int) -> int {
  return Builtin::mul_int32(lhs, rhs)
  // CHECK: mul i32
}
func [infix_left=200] / (lhs: int, rhs: int) -> int {
  return Builtin::sdiv_int32(lhs, rhs)
  // CHECK: sdiv i32
}
func [infix_left=200] % (lhs: int, rhs: int) -> int {
  return Builtin::srem_int32(lhs, rhs)
  // CHECK: srem i32
}
func [infix_left=190] + (lhs: int, rhs: int) -> int {
  return Builtin::add_int32(lhs, rhs)
  // CHECK: add i32
}
func [infix_left=190] - (lhs: int, rhs: int) -> int {
  return Builtin::sub_int32(lhs, rhs)
  // CHECK: sub i32
}
// In C, 180 is <<, >>
func [infix_left=170] < (lhs : int, rhs : int) -> bool {
  return Builtin::cmp_slt_int32(lhs, rhs)
  // CHECK: icmp slt i32
}
func [infix_left=170] > (lhs : int, rhs : int) -> bool {
  return Builtin::cmp_sgt_int32(lhs, rhs)
  // CHECK: icmp sgt i32
}
func [infix_left=170] <=(lhs : int, rhs : int) -> bool {
  return Builtin::cmp_sle_int32(lhs, rhs)
  // CHECK: icmp sle i32
}
func [infix_left=170] >=(lhs : int, rhs : int) -> bool {
  return Builtin::cmp_sge_int32(lhs, rhs)
  // CHECK: icmp sge i32
}
func [infix_left=160] ==(lhs : int, rhs : int) -> bool {
  return Builtin::cmp_eq_int32(lhs, rhs)
  // CHECK: icmp eq i32
}
func [infix_left=160] !=(lhs : int, rhs : int) -> bool {
  return Builtin::cmp_ne_int32(lhs, rhs)
  // CHECK: icmp ne i32
}
