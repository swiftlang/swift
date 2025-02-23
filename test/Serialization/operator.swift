// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/def_operator.swift
// RUN: llvm-bcanalyzer %t/def_operator.swiftmodule | %FileCheck %s
// RUN: %target-swift-frontend -typecheck -I%t %s
//
// Run with the interpreter using the proper filecheck pattern.
// RUN: %target-jit-run -I %t -DINTERP %s | %FileCheck --check-prefix=OUTPUT %s
// REQUIRES: swift_interpreter

// FIXME: iOS doesn't work because this test needs the interpreter to handle
// func typeCheckOnly (which causes link errors if built as an executable).

// CHECK-NOT: UnknownCode

import def_operator

prefix func ~~~(x: Int) -> (Int, Int, Int) {
  return (x, x, x)
}

var triple = (~~~42)
print("(\(triple.0), \(triple.1), \(triple.2))")
// OUTPUT: (42, 42, 42)

postfix func ^^(x: Int) -> Int {
  return x ^ x
}

print("\(1^^)")
// OUTPUT: 0


func *-(lhs: Int, rhs: Int) -> Int {
  return lhs - rhs
}
func -*(lhs: Int, rhs: Int) -> Int {
  return lhs - rhs
}
func *-*(lhs: Int, rhs: Int) -> Int {
  return lhs - rhs
}

print("\(5 *- 3 *- 2) \(5 -* 3 -* 2)")
// OUTPUT: 0 4
print("\(5 *- 3 -* 2) \(5 -* 3 *- 2)")
// OUTPUT: 0 4
print("\(5 *- 3 *-* 2) \(5 *-* 3 *- 2)")
// OUTPUT: 0 4

#if !INTERP

func typeCheckOnly() {
  ~~~true
  var b = false
  b^^
  true *-* false
  b *- false
}

#endif
