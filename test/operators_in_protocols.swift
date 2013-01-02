// RUN: %swift %s -verify
// XFAIL: *

protocol P {
  func [infix=180] << (lhs : This, rhs : This) -> This
  func [infix=180] >> (lhs : This, rhs : This) -> This
  func [assignment,infix_left=90] <<= (lhs : [byref] This, rhs : This)
  func [assignment,infix_left=90] >>= (lhs : [byref] This, rhs : This)
}
