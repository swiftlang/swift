class A {
  var a = 2
  subscript(_ : Int) -> Int {
    get {
      return 1
    }
    set {
      a = newValue
    }
  }
}

// RUN: %empty-directory(%t.result)
// RUN: %refactor -extract-function -source-filename %s -pos=5:1 -end-pos=5:15 >> %t.result/FromGetter.swift
// RUN: diff -u %S/Outputs/extract_subscript/FromGetter.swift.expected %t.result/FromGetter.swift
// RUN: %refactor -extract-function -source-filename %s -pos=8:1 -end-pos=8:19 >> %t.result/FromSetter.swift
// RUN: diff -u %S/Outputs/extract_subscript/FromSetter.swift.expected %t.result/FromSetter.swift
