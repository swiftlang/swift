func foo() -> Int{
  var aaa = 1 + 2
  aaa = aaa + 3
  if aaa == 3 { aaa = 4 }
  return aaa
}

// RUN: %empty-directory(%t.result)
// RUN: %refactor -extract-function -source-filename %s -pos=2:1 -end-pos=5:13 >> %t.result/L2-5.swift
// RUN: diff -u %S/Outputs/basic/L2-5.swift.expected %t.result/L2-5.swift
// RUN: %refactor -extract-function -source-filename %s -pos=3:1 -end-pos=5:13 >> %t.result/L3-5.swift
// RUN: diff -u %S/Outputs/basic/L3-5.swift.expected %t.result/L3-5.swift
// RUN: %refactor -extract-function -source-filename %s -pos=3:1 -end-pos=4:26 >> %t.result/L3-4.swift
// RUN: diff -u %S/Outputs/basic/L3-4.swift.expected %t.result/L3-4.swift
// REQUIRES: swift_swift_parser
