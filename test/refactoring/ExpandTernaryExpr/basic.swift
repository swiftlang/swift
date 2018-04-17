func testExpandBasicTernaryExpr() {
  let a = 3
  let b = 5
  let x = a < 5 ? a : b
}
func testExpandMultilineTernaryExpr() {
  let a = 3
  let b = 5
  let (x, y) = a < 5
    ? (a, b)
    : (b, a)
}
func testExpandAssignOnlyTernaryExpr() {
  let a = 3
  let b = 5
  let x: Int
  x = a < 5 ? a : b
}
func testExpandAssignOnlyTupleTernaryExpr() {
  let a = 3
  let b = 5
  let x: Int
  let y: Int
  (x, y) = a < 5 ? (a, b) : (b, a)
}

// RUN: %empty-directory(%t.result)

// RUN: %refactor -expand-ternary-expr -source-filename %s -pos=4:3 -end-pos=4:24 > %t.result/L4-3.swift
// RUN: diff -u %S/Outputs/basic/L4-3.swift.expected %t.result/L4-3.swift

// RUN: %refactor -expand-ternary-expr -source-filename %s -pos=9:3 -end-pos=11:13 > %t.result/L9-3.swift
// RUN: diff -u %S/Outputs/basic/L9-3.swift.expected %t.result/L9-3.swift

// RUN: %refactor -expand-ternary-expr -source-filename %s -pos=17:3 -end-pos=17:20 > %t.result/L17-3.swift
// RUN: diff -u %S/Outputs/basic/L17-3.swift.expected %t.result/L17-3.swift

// RUN: %refactor -expand-ternary-expr -source-filename %s -pos=24:3 -end-pos=24:35 > %t.result/L24-3.swift
// RUN: diff -u %S/Outputs/basic/L24-3.swift.expected %t.result/L24-3.swift
