class C {
  var Prop1 : Int {
    var a = 3 + 1
    return a
  }
  var Prop2 : Int {
    get { return 1 + 3 }
    set {}
  }
}

// RUN: %empty-directory(%t.result)
// RUN: %refactor -extract-function -source-filename %s -pos=3:13 -end-pos=3:18 >> %t.result/ImplicitGetter.swift
// RUN: diff -u %S/Outputs/extract_from_accessor/ImplicitGetter.swift.expected %t.result/ImplicitGetter.swift

// RUN: %refactor -extract-function -source-filename %s -pos=7:18 -end-pos=7:23 >> %t.result/ExplicitGetter.swift
// RUN: diff -u %S/Outputs/extract_from_accessor/ExplicitGetter.swift.expected %t.result/ExplicitGetter.swift
// REQUIRES: swift_swift_parser
