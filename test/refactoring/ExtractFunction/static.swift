class C {
  func foo1() -> Int {
    var a = 3 + 1
    a = 3
    return a
  }

  static func foo2() -> Int {
    var a = 3 + 1
    a = 3
    return a
  }
}

// RUN: %empty-directory(%t.result)
// RUN: %refactor -extract-function -source-filename %s -pos=3:1 -end-pos=5:13 >> %t.result/L3-5.swift
// RUN: diff -u %S/Outputs/static/L3-5.swift.expected %t.result/L3-5.swift
// RUN: %refactor -extract-function -source-filename %s -pos=9:1 -end-pos=11:13 >> %t.result/L9-11.swift
// RUN: diff -u %S/Outputs/static/L9-11.swift.expected %t.result/L9-11.swift
