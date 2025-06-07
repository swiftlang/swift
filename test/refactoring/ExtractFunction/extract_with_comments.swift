public class C {
public func foo() -> Int{
  var aaa = 1 + 2

  // this is comments
  aaa = aaa + 3
  if aaa == 3 { aaa = 4 }

  // we have comments here too
  return aaa
}
}

// RUN: %empty-directory(%t.result)
// RUN: %refactor -extract-function -source-filename %s -pos=6:1 -end-pos=7:26 >> %t.result/L6-7.swift
// RUN: diff -u %S/Outputs/extract_with_comments/L6-7.swift.expected %t.result/L6-7.swift
// REQUIRES: swift_swift_parser
