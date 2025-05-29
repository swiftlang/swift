// RUN: rm -rf %t.result && mkdir -p %t.result

struct User1: Codable {
  let firstName: String
  let lastName: String?

  init(from decoder: any Decoder) throws {
    firstName = ""
    lastName = ""
  }
  func encode(to encoder: any Encoder) throws {}
}

// RUN: %refactor -source-filename %s -pos=3:8 | %FileCheck %s
// CHECK-NOT: Add Explicit Codable

struct User2: Codable {
  let firstName: String
  let lastName: String?

  init(from decoder: any Decoder) throws {
    firstName = ""
    lastName = ""
  }
}

// RUN: %refactor -add-explicit-codable-implementation -source-filename %s -pos=17:8 > %t.result/has_encodable.swift
// RUN: diff -u %S/Outputs/existing/has_encodable.swift.expected %t.result/has_encodable.swift

struct User3: Codable {
  let firstName: String
  let lastName: String?

  func encode(to encoder: any Encoder) throws {}
}

// RUN: %refactor -add-explicit-codable-implementation -source-filename %s -pos=30:8 > %t.result/has_decodable.swift
// RUN: diff -u %S/Outputs/existing/has_decodable.swift.expected %t.result/has_decodable.swift
