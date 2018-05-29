func testConvertToTernaryExpr() {
  let a = 3
  let b = 5
  let x: Int
  if a < 5 {
    x = a
  } else {
    x = b
  }
}
func testConvertTupleToTernaryExpr() {
  let a = 3
  let b = 5
  let (x, y): (Int, Int)
  if a < 5 {
    (x, y) = (a, b)
  } else {
    (x, y) = (b, a)
  }
}

// RUN: %empty-directory(%t.result)

// RUN: %refactor -convert-to-ternary-expr -source-filename %s -pos=4:3 -end-pos=9:4 > %t.result/L4-3.swift
// RUN: diff -u %S/Outputs/basic/L4-3.swift.expected %t.result/L4-3.swift

// RUN: %refactor -convert-to-ternary-expr -source-filename %s -pos=5:3 -end-pos=9:4 > %t.result/L5-3.swift
// RUN: diff -u %S/Outputs/basic/L5-3.swift.expected %t.result/L5-3.swift

// RUN: %refactor -convert-to-ternary-expr -source-filename %s -pos=14:3 -end-pos=19:4 > %t.result/L14-3.swift
// RUN: diff -u %S/Outputs/basic/L14-3.swift.expected %t.result/L14-3.swift

// RUN: %refactor -convert-to-ternary-expr -source-filename %s -pos=15:3 -end-pos=19:4 > %t.result/L15-3.swift
// RUN: diff -u %S/Outputs/basic/L15-3.swift.expected %t.result/L15-3.swift
