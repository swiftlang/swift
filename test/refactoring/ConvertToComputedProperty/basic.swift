struct S {
  var field1 = 2
  var field2 = "2"
  var field3 = String()
  static var field4 = 4
  var y: Int! = 45
}

class C {
  static var field1 = S()
  public var field2 = 2
  private dynamic var field3 = 5
  @available(macOS 10.12, *) private static dynamic var field4 = 4
  let field5 = 5
}

// RUN: %empty-directory(%t.result)

// RUN: %refactor -convert-to-computed-property -source-filename %s -pos=2:3 -end-pos=2:17 > %t.result/L2-3.swift
// RUN: diff -u %S/Outputs/basic/L2-3.swift.expected %t.result/L2-3.swift

// RUN: %refactor -convert-to-computed-property -source-filename %s -pos=3:3 -end-pos=3:19 > %t.result/L3-3.swift
// RUN: diff -u %S/Outputs/basic/L3-3.swift.expected %t.result/L3-3.swift

// RUN: %refactor -convert-to-computed-property -source-filename %s -pos=4:3 -end-pos=4:24 > %t.result/L4-3.swift
// RUN: diff -u %S/Outputs/basic/L4-3.swift.expected %t.result/L4-3.swift

// RUN: %refactor -convert-to-computed-property -source-filename %s -pos=5:3 -end-pos=5:24 > %t.result/L5-3.swift
// RUN: diff -u %S/Outputs/basic/L5-3.swift.expected %t.result/L5-3.swift

// RUN: %refactor -convert-to-computed-property -source-filename %s -pos=6:3 -end-pos=6:19 > %t.result/L6-3.swift
// RUN: diff -u %S/Outputs/basic/L6-3.swift.expected %t.result/L6-3.swift

// RUN: %refactor -convert-to-computed-property -source-filename %s -pos=10:3 -end-pos=10:26 > %t.result/L10-3.swift
// RUN: diff -u %S/Outputs/basic/L10-3.swift.expected %t.result/L10-3.swift

// RUN: %refactor -convert-to-computed-property -source-filename %s -pos=11:3 -end-pos=11:24 > %t.result/L11-3.swift
// RUN: diff -u %S/Outputs/basic/L11-3.swift.expected %t.result/L11-3.swift

// RUN: %refactor -convert-to-computed-property -source-filename %s -pos=12:3 -end-pos=12:33 > %t.result/L12-3.swift
// RUN: diff -u %S/Outputs/basic/L12-3.swift.expected %t.result/L12-3.swift

// RUN: %refactor -convert-to-computed-property -source-filename %s -pos=13:3 -end-pos=13:67 > %t.result/L13-3.swift
// RUN: diff -u %S/Outputs/basic/L13-3.swift.expected %t.result/L13-3.swift

// RUN: %refactor -convert-to-computed-property -source-filename %s -pos=14:3 -end-pos=14:17 > %t.result/L14-3.swift
// RUN: diff -u %S/Outputs/basic/L14-3.swift.expected %t.result/L14-3.swift
