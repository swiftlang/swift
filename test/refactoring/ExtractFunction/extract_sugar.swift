func foo(_ a : inout [Int]) -> [Int] {
  a[0] = 3
  a[1] = 4
  a[3] = 4
  return a
}

// RUN: %empty-directory(%t.result)
// RUN: %refactor -extract-function -source-filename %s -pos=2:1 -end-pos=5:11 >> %t.result/L2-5.swift
// RUN: diff -u %S/Outputs/extract_sugar/L2-5.swift.expected %t.result/L2-5.swift
// REQUIRES: swift_swift_parser
