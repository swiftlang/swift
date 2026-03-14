// RUN: rm -rf %t.result && mkdir -p %t.result

struct User {
  let firstName: String
  let lastName: String?
}

extension User: Codable {
}

// RUN: %refactor -source-filename %s -pos=3:8 | %FileCheck %s
// RUN: %refactor -add-explicit-codable-implementation -source-filename %s -pos=8:11 > %t.result/basic.swift
// RUN: diff -u %S/Outputs/extension/basic.swift.expected %t.result/basic.swift

struct Generic<Value> {
  var value: Value
}

extension Generic {
}

extension Generic: Codable where Value: Codable {
}

// RUN: %refactor -source-filename %s -pos=15:8 | %FileCheck %s
// RUN: %refactor -source-filename %s -pos=19:8 | %FileCheck %s
// RUN: %refactor -add-explicit-codable-implementation -source-filename %s -pos=22:11 > %t.result/conditional.swift
// RUN: diff -u %S/Outputs/extension/conditional.swift.expected %t.result/conditional.swift

struct Outer {
  struct Inner {
    let value: Int
  }
}

extension Outer.Inner: Codable {
}

// RUN: %refactor -source-filename %s -pos=36:11 | %FileCheck %s
// RUN: %refactor -add-explicit-codable-implementation -source-filename %s -pos=36:17 > %t.result/nested.swift
// RUN: diff -u %S/Outputs/extension/nested.swift.expected %t.result/nested.swift

// CHECK-NOT: Add Explicit Codable
