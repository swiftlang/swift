func testCollapseNestedIf() {
  let a = 3
  if a > 2 {
    if a < 10 {}
  }
}
func testMultiConditionalNestedIf() {
  let a = 3
  if a > 2, a != 4 {
    if a < 10, a != 5 {}
  }
}
func testLetNestedIf() {
  let a: Int? = 3
  if let b = a {
    if b != 5 {}
  }
}
func testCaseLetNestedIf() {
  let a: Int? = 3
  if case .some(let b) = a {
    if b != 5 {}
  }
}
// RUN: %empty-directory(%t.result)
// RUN: %refactor -collapse-nested-if -source-filename %s -pos=3:3 > %t.result/L3-3.swift
// RUN: diff -u %S/Outputs/basic/L3-3.swift.expected %t.result/L3-3.swift
// RUN: %refactor -collapse-nested-if -source-filename %s -pos=9:3 > %t.result/L9-3.swift
// RUN: diff -u %S/Outputs/basic/L9-3.swift.expected %t.result/L9-3.swift
// RUN: %refactor -collapse-nested-if -source-filename %s -pos=15:3 > %t.result/L15-3.swift
// RUN: diff -u %S/Outputs/basic/L15-3.swift.expected %t.result/L15-3.swift
// RUN: %refactor -collapse-nested-if -source-filename %s -pos=21:3 > %t.result/L21-3.swift
// RUN: diff -u %S/Outputs/basic/L21-3.swift.expected %t.result/L21-3.swift
