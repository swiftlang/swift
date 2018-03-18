func foo1() -> Int { return 0 }

func foo(_ a : Int) -> Int {
  var aaa = 1 + 2 + 2 + a + foo1()
  return 1
}

// RUN: %empty-directory(%t.result)
// RUN: %refactor -extract-expr -source-filename %s -pos=4:13 -end-pos=4:18 >> %t.result/C13-18.swift
// RUN: diff -u %S/Outputs/basic/C13-18.swift.expected %t.result/C13-18.swift
// RUN: %refactor -extract-expr -source-filename %s -pos=4:13 -end-pos=4:26 >> %t.result/C13-26.swift
// RUN: diff -u %S/Outputs/basic/C13-26.swift.expected %t.result/C13-26.swift
// RUN: %refactor -extract-expr -source-filename %s -pos=4:13 -end-pos=4:35 >> %t.result/C13-35.swift
// RUN: diff -u %S/Outputs/basic/C13-35.swift.expected %t.result/C13-35.swift
