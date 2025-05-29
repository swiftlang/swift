public class C {
/// Insert before this.
public func foo() -> Int{
  var aaa = 1 + 2
  aaa = aaa + 3
  if aaa == 3 { aaa = 4 }
  return aaa
}
}

// RUN: %empty-directory(%t.result)
// RUN: %refactor -extract-function -source-filename %s -pos=5:1 -end-pos=6:26 >> %t.result/L5-6.swift
// RUN: diff -u %S/Outputs/extract_attributes/L5-6.swift.expected %t.result/L5-6.swift
// REQUIRES: swift_swift_parser
