class C {
  var a : Int
  var b : Int
  init(a : Int, b: Int) {
    self.a = a
    self.b = b
  }
}

// RUN: %empty-directory(%t.result)
// RUN: %refactor -extract-function -source-filename %s -pos=5:1 -end-pos=6:15 >> %t.result/L5-6.swift
// RUN: diff -u %S/Outputs/extract_init/L5-6.swift.expected %t.result/L5-6.swift
