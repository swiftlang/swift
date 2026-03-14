public class C {
/// Insert before this.
public func foo() -> Int{
  var aaa = 1 + 2
  aaa = aaa + 3
  if aaa == 3 { aaa = 4 }
  return aaa
}
public func new_name() {}
public func new_name1() {}
public func new_name2() {}
public func new_name3() {}
}

// RUN: %empty-directory(%t.result)
// RUN: %refactor -extract-function -source-filename %s -pos=5:1 -end-pos=6:26 >> %t.result/L5-6.swift
// RUN: diff -u %S/Outputs/name_correction/L5-6.swift.expected %t.result/L5-6.swift
// REQUIRES: swift_swift_parser
